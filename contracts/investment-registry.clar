
;; Investment Registry Contract
;; Tracks investor activity and provides analytics for the real estate platform

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVESTOR_NOT_FOUND (err u404))
(define-constant ERR_INVALID_AMOUNT (err u402))

;; Data Variables
(define-data-var total-investors uint u0)
(define-data-var total-investments uint u0)
(define-data-var total-investment-volume uint u0)

;; Data Maps
(define-map investor-profiles
  { investor: principal }
  {
    total-invested: uint,
    properties-count: uint,
    first-investment-block: uint,
    last-investment-block: uint,
    is-verified: bool
  }
)

(define-map investment-history
  { investor: principal, investment-id: uint }
  {
    property-id: uint,
    share-amount: uint,
    investment-amount: uint,
    block-height: uint,
    timestamp: uint
  }
)

(define-map investor-investment-count
  { investor: principal }
  { count: uint }
)

;; Read-Only Functions
(define-read-only (get-investor-profile (investor principal))
  (map-get? investor-profiles { investor: investor })
)

(define-read-only (get-investment-record (investor principal) (investment-id uint))
  (map-get? investment-history { investor: investor, investment-id: investment-id })
)

(define-read-only (get-investor-investment-count (investor principal))
  (default-to u0 (get count (map-get? investor-investment-count { investor: investor })))
)

(define-read-only (get-platform-stats)
  (ok {
    total-investors: (var-get total-investors),
    total-investments: (var-get total-investments),
    total-volume: (var-get total-investment-volume)
  })
)

(define-read-only (is-investor-verified (investor principal))
  (match (get-investor-profile investor)
    profile (get is-verified profile)
    false
  )
)

;; Public Functions
(define-public (register-investment 
  (investor principal)
  (property-id uint)
  (share-amount uint)
  (investment-amount uint)
)
  (let (
    (current-profile (default-to 
      { total-invested: u0, properties-count: u0, first-investment-block: stacks-block-height, last-investment-block: u0, is-verified: false }
      (get-investor-profile investor)
    ))
    (investment-count (get-investor-investment-count investor))
    (is-first-investment (is-eq (get total-invested current-profile) u0))
  )
    ;; Record investment history
    (map-set investment-history
      { investor: investor, investment-id: investment-count }
      {
        property-id: property-id,
        share-amount: share-amount,
        investment-amount: investment-amount,
        block-height: stacks-block-height,
        timestamp: stacks-block-height
      }
    )
    
    ;; Update investor profile
    (map-set investor-profiles
      { investor: investor }
      {
        total-invested: (+ (get total-invested current-profile) investment-amount),
        properties-count: (if is-first-investment 
          (+ (get properties-count current-profile) u1)
          (get properties-count current-profile)
        ),
        first-investment-block: (get first-investment-block current-profile),
        last-investment-block: stacks-block-height,
        is-verified: (get is-verified current-profile)
      }
    )
    
    ;; Update investment count
    (map-set investor-investment-count
      { investor: investor }
      { count: (+ investment-count u1) }
    )
    
    ;; Update platform stats
    (if is-first-investment
      (var-set total-investors (+ (var-get total-investors) u1))
      true
    )
    (var-set total-investments (+ (var-get total-investments) u1))
    (var-set total-investment-volume (+ (var-get total-investment-volume) investment-amount))
    
    (ok true)
  )
)

(define-public (verify-investor (investor principal))
  (let (
    (profile (unwrap! (get-investor-profile investor) ERR_INVESTOR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    
    (map-set investor-profiles
      { investor: investor }
      (merge profile { is-verified: true })
    )
    
    (ok true)
  )
)

(define-public (update-property-count (investor principal) (new-count uint))
  (let (
    (profile (unwrap! (get-investor-profile investor) ERR_INVESTOR_NOT_FOUND))
  )
    (map-set investor-profiles
      { investor: investor }
      (merge profile { properties-count: new-count })
    )
    
    (ok true)
  )
)

