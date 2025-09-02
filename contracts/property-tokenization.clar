;; Property Tokenization Contract
;; Enables fractional ownership of commercial real estate

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_INVALID_AMOUNT (err u402))
(define-constant ERR_PROPERTY_NOT_FOUND (err u404))
(define-constant ERR_INSUFFICIENT_BALANCE (err u403))
(define-constant ERR_PROPERTY_NOT_ACTIVE (err u405))
(define-constant ERR_INVALID_SHARE_AMOUNT (err u406))
(define-constant ERR_PROPERTY_EXISTS (err u407))

;; Data Variables
(define-data-var next-property-id uint u1)

;; Data Maps
(define-map properties
  { property-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    total-value: uint,
    total-shares: uint,
    shares-sold: uint,
    price-per-share: uint,
    owner: principal,
    is-active: bool
  }
)

(define-map investor-shares
  { property-id: uint, investor: principal }
  { shares: uint }
)

(define-map investor-portfolios
  { investor: principal }
  { total-properties: uint }
)

;; Read-Only Functions
(define-read-only (get-property (property-id uint))
  (map-get? properties { property-id: property-id })
)

(define-read-only (get-investor-shares (property-id uint) (investor principal))
  (default-to u0 (get shares (map-get? investor-shares { property-id: property-id, investor: investor })))
)

(define-read-only (get-investor-portfolio (investor principal))
  (map-get? investor-portfolios { investor: investor })
)

(define-read-only (get-next-property-id)
  (var-get next-property-id)
)

(define-read-only (calculate-investment-value (property-id uint) (investor principal))
  (let (
    (property (unwrap! (get-property property-id) (err u404)))
    (shares-owned (get-investor-shares property-id investor))
  )
    (ok (* shares-owned (get price-per-share property)))
  )
)

;; Public Functions
(define-public (create-property 
  (name (string-ascii 50))
  (location (string-ascii 100))
  (total-value uint)
  (total-shares uint)
)
  (let (
    (property-id (var-get next-property-id))
    (price-per-share (/ total-value total-shares))
  )
    (asserts! (> total-value u0) ERR_INVALID_AMOUNT)
    (asserts! (> total-shares u0) ERR_INVALID_SHARE_AMOUNT)
    
    (map-set properties
      { property-id: property-id }
      {
        name: name,
        location: location,
        total-value: total-value,
        total-shares: total-shares,
        shares-sold: u0,
        price-per-share: price-per-share,
        owner: tx-sender,
        is-active: true
      }
    )
    
    (var-set next-property-id (+ property-id u1))
    (ok property-id)
  )
)

(define-public (invest-in-property (property-id uint) (share-amount uint))
  (let (
    (property (unwrap! (get-property property-id) ERR_PROPERTY_NOT_FOUND))
    (current-shares (get-investor-shares property-id tx-sender))
    (total-cost (* share-amount (get price-per-share property)))
    (new-shares-sold (+ (get shares-sold property) share-amount))
  )
    (asserts! (get is-active property) ERR_PROPERTY_NOT_ACTIVE)
    (asserts! (> share-amount u0) ERR_INVALID_SHARE_AMOUNT)
    (asserts! (<= new-shares-sold (get total-shares property)) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer STX payment to property owner
    (try! (stx-transfer? total-cost tx-sender (get owner property)))
    
    ;; Update investor shares
    (map-set investor-shares
      { property-id: property-id, investor: tx-sender }
      { shares: (+ current-shares share-amount) }
    )
    
    ;; Update property shares sold
    (map-set properties
      { property-id: property-id }
      (merge property { shares-sold: new-shares-sold })
    )
    
    ;; Update investor portfolio
    (let (
      (portfolio (default-to { total-properties: u0 } (get-investor-portfolio tx-sender)))
      (is-new-investment (is-eq current-shares u0))
    )
      (if is-new-investment
        (map-set investor-portfolios
          { investor: tx-sender }
          { total-properties: (+ (get total-properties portfolio) u1) }
        )
        true
      )
    )
    
    (ok share-amount)
  )
)

(define-public (transfer-shares (property-id uint) (recipient principal) (share-amount uint))
  (let (
    (sender-shares (get-investor-shares property-id tx-sender))
    (recipient-shares (get-investor-shares property-id recipient))
  )
    (asserts! (>= sender-shares share-amount) ERR_INSUFFICIENT_BALANCE)
    (asserts! (> share-amount u0) ERR_INVALID_SHARE_AMOUNT)
    
    ;; Update sender shares
    (map-set investor-shares
      { property-id: property-id, investor: tx-sender }
      { shares: (- sender-shares share-amount) }
    )
    
    ;; Update recipient shares
    (map-set investor-shares
      { property-id: property-id, investor: recipient }
      { shares: (+ recipient-shares share-amount) }
    )
    
    (ok true)
  )
)

(define-public (deactivate-property (property-id uint))
  (let (
    (property (unwrap! (get-property property-id) ERR_PROPERTY_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get owner property)) ERR_UNAUTHORIZED)
    
    (map-set properties
      { property-id: property-id }
      (merge property { is-active: false })
    )
    
    (ok true)
  )
)

