;; Ocean Cleanup Incentive Network
;; Reward ocean cleanup efforts with tradeable environmental tokens

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_CLEANUP (err u103))
(define-constant ERR_CLEANUP_ALREADY_VERIFIED (err u104))
(define-constant ERR_NOT_FOUND (err u105))
(define-constant ERR_INVALID_VERIFIER (err u106))
(define-constant ERR_SELF_TRADE (err u107))

;; Define the fungible token
(define-fungible-token ocean-clean-token)

;; Data Variables
(define-data-var token-name (string-ascii 32) "Ocean Clean Token")
(define-data-var token-symbol (string-ascii 10) "OCT")
(define-data-var total-supply uint u0)
(define-data-var next-cleanup-id uint u1)
(define-data-var cleanup-reward-rate uint u100) ;; tokens per kg of waste

;; Data Maps
(define-map balances principal uint)
(define-map allowances {owner: principal, spender: principal} uint)

;; Cleanup Event Structure
(define-map cleanup-events 
    uint 
    {
        cleaner: principal,
        location: (string-ascii 50),
        waste-amount: uint, ;; in kg
        verification-status: (string-ascii 20),
        verifier: (optional principal),
        timestamp: uint,
        reward-amount: uint,
        gps-coords: (string-ascii 30)
    }
)

;; Authorized Verifiers
(define-map authorized-verifiers principal bool)

;; Trading Orders
(define-map trade-orders 
    uint 
    {
        seller: principal,
        amount: uint,
        price-per-token: uint,
        active: bool,
        created-at: uint
    }
)

(define-data-var next-order-id uint u1)

;; SIP-010 Token Standard Functions

(define-public (transfer (amount uint) (from principal) (to principal) (memo (optional (buff 34))))
    (begin
        (asserts! (or (is-eq from tx-sender) (is-eq from CONTRACT_OWNER)) ERR_UNAUTHORIZED)
        (asserts! (>= (get-balance from) amount) ERR_INSUFFICIENT_BALANCE)
        (try! (ft-transfer? ocean-clean-token amount from to))
        (print memo)
        (ok true)
    )
)

(define-read-only (get-name)
    (ok (var-get token-name))
)

(define-read-only (get-symbol)
    (ok (var-get token-symbol))
)

(define-read-only (get-decimals)
    (ok u6)
)

(define-read-only (get-balance (user principal))
    (ft-get-balance ocean-clean-token user)
)

(define-read-only (get-total-supply)
    (ok (ft-get-supply ocean-clean-token))
)

(define-read-only (get-token-uri)
    (ok (some "https://oceancleanup.network/token-metadata"))
)

;; Core Contract Functions

(define-public (submit-cleanup (location (string-ascii 50)) (waste-amount uint) (gps-coords (string-ascii 30)))
    (let 
        (
            (cleanup-id (var-get next-cleanup-id))
            (reward-amount (* waste-amount (var-get cleanup-reward-rate)))
        )
        (asserts! (> waste-amount u0) ERR_INVALID_AMOUNT)
        (map-set cleanup-events cleanup-id {
            cleaner: tx-sender,
            location: location,
            waste-amount: waste-amount,
            verification-status: "pending",
            verifier: none,
            timestamp: block-height,
            reward-amount: reward-amount,
            gps-coords: gps-coords
        })
        (var-set next-cleanup-id (+ cleanup-id u1))
        (print {event: "cleanup-submitted", cleanup-id: cleanup-id, cleaner: tx-sender})
        (ok cleanup-id)
    )
)

(define-public (verify-cleanup (cleanup-id uint) (approved bool))
    (let 
        (
            (cleanup-data (unwrap! (map-get? cleanup-events cleanup-id) ERR_NOT_FOUND))
            (verifier tx-sender)
        )
        (asserts! (default-to false (map-get? authorized-verifiers verifier)) ERR_INVALID_VERIFIER)
        (asserts! (is-eq (get verification-status cleanup-data) "pending") ERR_CLEANUP_ALREADY_VERIFIED)
        
        (if approved
            (begin
                ;; Approve cleanup and mint tokens
                (map-set cleanup-events cleanup-id (merge cleanup-data {
                    verification-status: "approved",
                    verifier: (some verifier)
                }))
                (try! (mint-tokens (get cleaner cleanup-data) (get reward-amount cleanup-data)))
                (print {event: "cleanup-approved", cleanup-id: cleanup-id})
                (ok true)
            )
            (begin
                ;; Reject cleanup
                (map-set cleanup-events cleanup-id (merge cleanup-data {
                    verification-status: "rejected",
                    verifier: (some verifier)
                }))
                (print {event: "cleanup-rejected", cleanup-id: cleanup-id})
                (ok true)
            )
        )
    )
)

(define-private (mint-tokens (recipient principal) (amount uint))
    (begin
        (try! (ft-mint? ocean-clean-token amount recipient))
        (print {event: "tokens-minted", recipient: recipient, amount: amount})
        (ok true)
    )
)

;; Trading Functions

(define-public (create-sell-order (amount uint) (price-per-token uint))
    (let 
        (
            (order-id (var-get next-order-id))
        )
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (> price-per-token u0) ERR_INVALID_AMOUNT)
        (asserts! (>= (get-balance tx-sender) amount) ERR_INSUFFICIENT_BALANCE)
        
        (map-set trade-orders order-id {
            seller: tx-sender,
            amount: amount,
            price-per-token: price-per-token,
            active: true,
            created-at: block-height
        })
        (var-set next-order-id (+ order-id u1))
        (print {event: "sell-order-created", order-id: order-id, seller: tx-sender})
        (ok order-id)
    )
)

(define-public (buy-tokens (order-id uint) (token-amount uint))
    (let 
        (
            (order-data (unwrap! (map-get? trade-orders order-id) ERR_NOT_FOUND))
            (total-cost (* token-amount (get price-per-token order-data)))
            (seller (get seller order-data))
        )
        (asserts! (get active order-data) ERR_NOT_FOUND)
        (asserts! (not (is-eq tx-sender seller)) ERR_SELF_TRADE)
        (asserts! (<= token-amount (get amount order-data)) ERR_INVALID_AMOUNT)
        (asserts! (>= (get-balance seller) token-amount) ERR_INSUFFICIENT_BALANCE)
        
        ;; Transfer STX from buyer to seller
        (try! (stx-transfer? total-cost tx-sender seller))
        
        ;; Transfer tokens from seller to buyer
        (try! (ft-transfer? ocean-clean-token token-amount seller tx-sender))
        
        ;; Update order
        (if (is-eq token-amount (get amount order-data))
            (map-set trade-orders order-id (merge order-data {active: false}))
            (map-set trade-orders order-id (merge order-data {amount: (- (get amount order-data) token-amount)}))
        )
        
        (print {event: "tokens-purchased", order-id: order-id, buyer: tx-sender, amount: token-amount})
        (ok true)
    )
)

(define-public (cancel-sell-order (order-id uint))
    (let 
        (
            (order-data (unwrap! (map-get? trade-orders order-id) ERR_NOT_FOUND))
        )
        (asserts! (is-eq tx-sender (get seller order-data)) ERR_UNAUTHORIZED)
        (asserts! (get active order-data) ERR_NOT_FOUND)
        
        (map-set trade-orders order-id (merge order-data {active: false}))
        (print {event: "sell-order-cancelled", order-id: order-id})
        (ok true)
    )
)

;; Admin Functions

(define-public (add-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set authorized-verifiers verifier true)
        (print {event: "verifier-added", verifier: verifier})
        (ok true)
    )
)

(define-public (remove-verifier (verifier principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set authorized-verifiers verifier false)
        (print {event: "verifier-removed", verifier: verifier})
        (ok true)
    )
)

(define-public (update-reward-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
        (var-set cleanup-reward-rate new-rate)
        (print {event: "reward-rate-updated", new-rate: new-rate})
        (ok true)
    )
)

;; Read-only Functions

(define-read-only (get-cleanup-details (cleanup-id uint))
    (map-get? cleanup-events cleanup-id)
)

(define-read-only (get-trade-order (order-id uint))
    (map-get? trade-orders order-id)
)

(define-read-only (is-authorized-verifier (user principal))
    (default-to false (map-get? authorized-verifiers user))
)

(define-read-only (get-reward-rate)
    (var-get cleanup-reward-rate)
)

(define-read-only (get-cleanup-stats (cleaner principal))
    (let 
        (
            (balance (get-balance cleaner))
        )
        {
            token-balance: balance,
            estimated-waste-cleaned: (/ balance (var-get cleanup-reward-rate))
        }
    )
)

;; Initialize contract
(begin
    (map-set authorized-verifiers CONTRACT_OWNER true)
    (print {event: "contract-initialized", owner: CONTRACT_OWNER})
)