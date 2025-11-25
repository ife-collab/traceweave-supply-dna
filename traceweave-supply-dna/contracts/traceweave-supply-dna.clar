;; TraceWeave Product DNA Smart Contract
;; Core contract for creating and managing Living Product Identities on the blockchain

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-data (err u104))

;; Data Variables
(define-data-var next-product-id uint u1)
(define-data-var platform-active bool true)

;; Product DNA Structure
(define-map products
  { product-id: uint }
  {
    owner: principal,
    manufacturer: principal,
    product-type: (string-ascii 50),
    created-at: uint,
    current-location: (string-ascii 100),
    sustainability-score: uint,
    carbon-footprint: uint,
    lifecycle-stage: (string-ascii 20),
    is-active: bool
  }
)

;; Supply Chain Event Log
(define-map supply-chain-events
  { product-id: uint, event-id: uint }
  {
    timestamp: uint,
    event-type: (string-ascii 50),
    location: (string-ascii 100),
    actor: principal,
    data-hash: (buff 32),
    verified: bool
  }
)

;; Track event count per product
(define-map product-event-count
  { product-id: uint }
  { count: uint }
)

;; Participant Reputation System
(define-map participant-reputation
  { participant: principal }
  {
    transparency-score: uint,
    sustainability-contributions: uint,
    total-interactions: uint,
    governance-tokens: uint
  }
)

;; Circular Economy Incentives
(define-map recycling-rewards
  { product-id: uint }
  {
    end-of-life-value: uint,
    recycling-bonus: uint,
    carbon-credit: uint
  }
)

;; Read-only functions

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-supply-chain-event (product-id uint) (event-id uint))
  (map-get? supply-chain-events { product-id: product-id, event-id: event-id })
)

(define-read-only (get-participant-reputation (participant principal))
  (map-get? participant-reputation { participant: participant })
)

(define-read-only (get-recycling-reward (product-id uint))
  (map-get? recycling-rewards { product-id: product-id })
)

(define-read-only (get-product-event-count (product-id uint))
  (default-to { count: u0 } (map-get? product-event-count { product-id: product-id }))
)

(define-read-only (is-platform-active)
  (var-get platform-active)
)

;; Public functions

;; Create new Product DNA
(define-public (create-product 
    (product-type (string-ascii 50))
    (initial-location (string-ascii 100))
    (initial-carbon-footprint uint))
  (let
    (
      (product-id (var-get next-product-id))
    )
    (asserts! (var-get platform-active) err-owner-only)
    (asserts! (> (len product-type) u0) err-invalid-data)
    
    (map-set products
      { product-id: product-id }
      {
        owner: tx-sender,
        manufacturer: tx-sender,
        product-type: product-type,
        created-at: block-height,
        current-location: initial-location,
        sustainability-score: u50,
        carbon-footprint: initial-carbon-footprint,
        lifecycle-stage: "production",
        is-active: true
      }
    )
    
    ;; Initialize event counter
    (map-set product-event-count
      { product-id: product-id }
      { count: u0 }
    )
    
    ;; Initialize recycling rewards
    (map-set recycling-rewards
      { product-id: product-id }
      {
        end-of-life-value: u0,
        recycling-bonus: u0,
        carbon-credit: u0
      }
    )
    
    ;; Update reputation for manufacturer
    (update-reputation tx-sender u1 u0)
    
    (var-set next-product-id (+ product-id u1))
    (ok product-id)
  )
)

;; Transfer product ownership
(define-public (transfer-product (product-id uint) (new-owner principal))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
    )
    (asserts! (is-eq (get owner product) tx-sender) err-unauthorized)
    (asserts! (get is-active product) err-invalid-data)
    
    (map-set products
      { product-id: product-id }
      (merge product { owner: new-owner })
    )
    
    ;; Log transfer event
    (try! (log-supply-chain-event 
      product-id 
      "transfer" 
      (get current-location product)
      0x0000000000000000000000000000000000000000000000000000000000000000))
    
    (ok true)
  )
)

;; Update product location and sustainability metrics
(define-public (update-product-status
    (product-id uint)
    (new-location (string-ascii 100))
    (new-lifecycle-stage (string-ascii 20))
    (sustainability-delta int)
    (carbon-delta uint))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
      (current-score (get sustainability-score product))
      (new-score (if (< sustainability-delta 0)
                    (if (>= current-score (to-uint (- 0 sustainability-delta)))
                      (- current-score (to-uint (- 0 sustainability-delta)))
                      u0)
                    (+ current-score (to-uint sustainability-delta))))
    )
    (asserts! (is-eq (get owner product) tx-sender) err-unauthorized)
    (asserts! (get is-active product) err-invalid-data)
    
    (map-set products
      { product-id: product-id }
      (merge product {
        current-location: new-location,
        lifecycle-stage: new-lifecycle-stage,
        sustainability-score: new-score,
        carbon-footprint: (+ (get carbon-footprint product) carbon-delta)
      })
    )
    
    ;; Update reputation
    (update-reputation tx-sender u0 u1)
    
    (ok true)
  )
)

;; Log supply chain event
(define-public (log-supply-chain-event
    (product-id uint)
    (event-type (string-ascii 50))
    (location (string-ascii 100))
    (data-hash (buff 32)))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
      (event-counter (get count (get-product-event-count product-id)))
      (new-event-id (+ event-counter u1))
    )
    (asserts! (get is-active product) err-invalid-data)
    
    (map-set supply-chain-events
      { product-id: product-id, event-id: new-event-id }
      {
        timestamp: block-height,
        event-type: event-type,
        location: location,
        actor: tx-sender,
        data-hash: data-hash,
        verified: false
      }
    )
    
    ;; Update event counter
    (map-set product-event-count
      { product-id: product-id }
      { count: new-event-id }
    )
    
    ;; Update reputation
    (update-reputation tx-sender u0 u1)
    
    (ok new-event-id)
  )
)

;; Verify supply chain event
(define-public (verify-event (product-id uint) (event-id uint))
  (let
    (
      (event (unwrap! (get-supply-chain-event product-id event-id) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (map-set supply-chain-events
      { product-id: product-id, event-id: event-id }
      (merge event { verified: true })
    )
    
    (ok true)
  )
)

;; Mark product for recycling and calculate rewards
(define-public (initiate-recycling (product-id uint) (recycling-bonus uint))
  (let
    (
      (product (unwrap! (get-product product-id) err-not-found))
      (sustainability (get sustainability-score product))
      (carbon-credit (/ (* sustainability u10) u100))
    )
    (asserts! (is-eq (get owner product) tx-sender) err-unauthorized)
    (asserts! (get is-active product) err-invalid-data)
    
    (map-set products
      { product-id: product-id }
      (merge product { 
        lifecycle-stage: "recycling",
        is-active: false 
      })
    )
    
    (map-set recycling-rewards
      { product-id: product-id }
      {
        end-of-life-value: recycling-bonus,
        recycling-bonus: recycling-bonus,
        carbon-credit: carbon-credit
      }
    )
    
    ;; Award governance tokens for recycling
    (update-reputation tx-sender u10 u5)
    
    (ok { recycling-bonus: recycling-bonus, carbon-credit: carbon-credit })
  )
)

;; Private functions

(define-private (update-reputation 
    (participant principal) 
    (transparency-increase uint)
    (sustainability-increase uint))
  (let
    (
      (current-rep (default-to 
        { transparency-score: u0, sustainability-contributions: u0, total-interactions: u0, governance-tokens: u0 }
        (get-participant-reputation participant)))
    )
    (map-set participant-reputation
      { participant: participant }
      {
        transparency-score: (+ (get transparency-score current-rep) transparency-increase),
        sustainability-contributions: (+ (get sustainability-contributions current-rep) sustainability-increase),
        total-interactions: (+ (get total-interactions current-rep) u1),
        governance-tokens: (+ (get governance-tokens current-rep) (+ transparency-increase sustainability-increase))
      }
    )
    true
  )
)

;; Admin functions

(define-public (toggle-platform (active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set platform-active active)
    (ok true)
  )
)