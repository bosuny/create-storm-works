;; CreateStorm - Simplified Creative Asset Management Platform

;; Error Constants
(define-constant ERR_NOT_AUTHORIZED (err u1000))
(define-constant ERR_ASSET_NOT_FOUND (err u1001))
(define-constant ERR_INVALID_PERCENTAGE (err u1002))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u1003))
(define-constant ERR_INVALID_TITLE (err u1004))
(define-constant ERR_INVALID_LICENSE_TYPE (err u1005))
(define-constant ERR_PARENT_NOT_FOUND (err u1006))

;; Contract variables
(define-data-var contract_owner principal tx-sender)
(define-data-var platform_fee_rate uint u250) ;; 2.5% (250/10000)
(define-data-var next_asset_id uint u1)

;; License types (simplified)
(define-constant LICENSE_PERSONAL u1)
(define-constant LICENSE_COMMERCIAL u2)
(define-constant LICENSE_FULL u3)

;; Creative Asset Structure (simplified)
(define-map assets
  { asset_id: uint }
  {
    creator: principal,
    title: (string-ascii 64),
    parent_asset: (optional uint),
    creation_time: uint,
    license_type: uint,
    total_revenue: uint
  }
)

;; License Pricing
(define-map license_prices
  { asset_id: uint }
  { price: uint, sales: uint }
)

;; Revenue splits for derivative assets
(define-map revenue_shares
  { asset_id: uint, recipient: principal }
  { percentage: uint }
)

;; Helper Functions
(define-private (is_valid_license_type (license_type uint))
  (and (>= license_type LICENSE_PERSONAL) (<= license_type LICENSE_FULL))
)

(define-private (is_valid_percentage (percentage uint))
  (and (>= percentage u1) (<= percentage u100))
)

(define-private (calculate_platform_fee (amount uint))
  (/ (* amount (var-get platform_fee_rate)) u10000)
)

;; Admin Functions
(define-public (set_platform_fee (new_fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract_owner)) ERR_NOT_AUTHORIZED)
    (asserts! (<= new_fee u1000) ERR_INVALID_PERCENTAGE) ;; Max 10%
    (var-set platform_fee_rate new_fee)
    (ok true)
  )
)

;; Core Functions
(define-public (create_asset 
  (title (string-ascii 64))
  (license_type uint)
)
  (let
    (
      (asset_id (var-get next_asset_id))
    )
    ;; Validations
    (asserts! (> (len title) u0) ERR_INVALID_TITLE)
    (asserts! (<= (len title) u64) ERR_INVALID_TITLE)
    (asserts! (is_valid_license_type license_type) ERR_INVALID_LICENSE_TYPE)
    
    ;; Create asset
    (map-set assets
      { asset_id: asset_id }
      {
        creator: tx-sender,
        title: title,
        parent_asset: none,
        creation_time: burn-block-height,
        license_type: license_type,
        total_revenue: u0
      }
    )
    
    ;; Set initial revenue share (100% to creator)
    (map-set revenue_shares
      { asset_id: asset_id, recipient: tx-sender }
      { percentage: u100 }
    )
    
    ;; Update next asset ID
    (var-set next_asset_id (+ asset_id u1))
    (ok asset_id)
  )
)

(define-public (create_derivative_asset
  (parent_id uint)
  (title (string-ascii 64))
  (creator_share uint)
)
  (let
    (
      (asset_id (var-get next_asset_id))
      (parent_asset (unwrap! (map-get? assets { asset_id: parent_id }) ERR_PARENT_NOT_FOUND))
      (parent_share (- u100 creator_share))
    )
    ;; Validations
    (asserts! (> (len title) u0) ERR_INVALID_TITLE)
    (asserts! (<= (len title) u64) ERR_INVALID_TITLE)
    (asserts! (is_valid_percentage creator_share) ERR_INVALID_PERCENTAGE)
    (asserts! (< creator_share u100) ERR_INVALID_PERCENTAGE) ;; Must leave some for parent
    
    ;; Create derivative asset
    (map-set assets
      { asset_id: asset_id }
      {
        creator: tx-sender,
        title: title,
        parent_asset: (some parent_id),
        creation_time: burn-block-height,
        license_type: (get license_type parent_asset),
        total_revenue: u0
      }
    )
    
    ;; Set revenue shares
    (map-set revenue_shares
      { asset_id: asset_id, recipient: tx-sender }
      { percentage: creator_share }
    )
    
    (map-set revenue_shares
      { asset_id: asset_id, recipient: (get creator parent_asset) }
      { percentage: parent_share }
    )
    
    ;; Update next asset ID
    (var-set next_asset_id (+ asset_id u1))
    (ok asset_id)
  )
)

(define-public (set_license_price
  (asset_id uint)
  (price uint)
)
  (let
    (
      (asset (unwrap! (map-get? assets { asset_id: asset_id }) ERR_ASSET_NOT_FOUND))
    )
    ;; Only creator can set price
    (asserts! (is-eq tx-sender (get creator asset)) ERR_NOT_AUTHORIZED)
    
    ;; Set price
    (map-set license_prices
      { asset_id: asset_id }
      { 
        price: price, 
        sales: (default-to u0 (get sales (map-get? license_prices { asset_id: asset_id })))
      }
    )
    
    (ok true)
  )
)

(define-public (purchase_license
  (asset_id uint)
)
  (let
    (
      (asset (unwrap! (map-get? assets { asset_id: asset_id }) ERR_ASSET_NOT_FOUND))
      (pricing (unwrap! (map-get? license_prices { asset_id: asset_id }) ERR_ASSET_NOT_FOUND))
      (price (get price pricing))
      (fee_amount (calculate_platform_fee price))
      (net_amount (- price fee_amount))
    )
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? fee_amount tx-sender (var-get contract_owner)))
    
    ;; Distribute remaining amount based on revenue shares
    (let
      (
        (creator_share (default-to { percentage: u100 } 
          (map-get? revenue_shares { asset_id: asset_id, recipient: (get creator asset) })))
        (creator_amount (/ (* net_amount (get percentage creator_share)) u100))
      )
      ;; Transfer to creator
      (try! (stx-transfer? creator_amount tx-sender (get creator asset)))
      
      ;; If there's a parent asset, transfer remaining to parent creator
      (match (get parent_asset asset)
        parent_id
          (let
            (
              (parent_asset_data (unwrap-panic (map-get? assets { asset_id: parent_id })))
              (parent_amount (- net_amount creator_amount))
            )
            (if (> parent_amount u0)
              (try! (stx-transfer? parent_amount tx-sender (get creator parent_asset_data)))
              true
            )
          )
        true
      )
    )
    
    ;; Update sales count
    (map-set license_prices
      { asset_id: asset_id }
      { 
        price: price,
        sales: (+ (get sales pricing) u1)
      }
    )
    
    ;; Update total revenue
    (map-set assets
      { asset_id: asset_id }
      (merge asset { total_revenue: (+ (get total_revenue asset) price) })
    )
    
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get_asset (asset_id uint))
  (map-get? assets { asset_id: asset_id })
)

(define-read-only (get_asset_price (asset_id uint))
  (map-get? license_prices { asset_id: asset_id })
)

(define-read-only (get_revenue_share (asset_id uint) (recipient principal))
  (map-get? revenue_shares { asset_id: asset_id, recipient: recipient })
)

(define-read-only (get_platform_fee)
  (var-get platform_fee_rate)
)

(define-read-only (get_next_asset_id)
  (var-get next_asset_id)
)

(define-read-only (get_contract_owner)
  (var-get contract_owner)
)