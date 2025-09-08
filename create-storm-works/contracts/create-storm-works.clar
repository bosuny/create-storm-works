;; CreateStorm - Dynamic Creative Asset Management Platform
;; Revolutionizing creative asset management through Dynamic Provenance Chains

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-ASSET-NOT-FOUND (err u1001))
(define-constant ERR-INVALID-PERCENTAGE (err u1002))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u1003))
(define-constant ERR-ASSET-ALREADY-EXISTS (err u1004))
(define-constant ERR-INVALID-CONTRIBUTOR (err u1005))
(define-constant ERR-REVENUE-SPLIT-ERROR (err u1006))
(define-constant ERR-INVALID-LICENSE-TYPE (err u1007))
(define-constant ERR-PARENT-NOT-FOUND (err u1008))
(define-constant ERR-INVALID-PROVENANCE (err u1009))
(define-constant ERR-COLLABORATION-FULL (err u1010))
(define-constant ERR-ORACLE-VIOLATION (err u1011))

;; Contract owner and admin
(define-data-var contract-owner principal tx-sender)
(define-data-var platform-fee-percentage uint u250) ;; 2.5%
(define-data-var next-asset-id uint u1)
(define-data-var oracle-threshold uint u70) ;; 70% similarity threshold

;; License types
(define-constant LICENSE-PERSONAL u1)
(define-constant LICENSE-COMMERCIAL u2)
(define-constant LICENSE-DERIVATIVE u3)

;; Creative Asset Structure
(define-map creative-assets
  { asset-id: uint }
  {
    creator: principal,
    title: (string-ascii 100),
    provenance-dna: (buff 32),
    parent-asset: (optional uint),
    creative-distance: uint,
    total-revenue: uint,
    creation-timestamp: uint,
    license-types: uint,
    is-collaborative: bool,
    derivative-count: uint
  }
)

;; Provenance Chain Tracking
(define-map provenance-chains
  { asset-id: uint }
  {
    inspiration-sources: (list 10 uint),
    contributor-weights: (list 10 uint),
    genealogy-depth: uint,
    attribution-vector: (buff 20)
  }
)

;; Revenue Distribution Maps
(define-map revenue-splits
  { asset-id: uint, beneficiary: principal }
  { percentage: uint, total-earned: uint }
)

;; Collaborative Creation Pools
(define-map collaboration-pools
  { pool-id: uint }
  {
    contributors: (list 20 principal),
    contribution-percentages: (list 20 uint),
    pool-balance: uint,
    is-active: bool,
    creation-deadline: uint
  }
)

;; License Pricing
(define-map license-pricing
  { asset-id: uint, license-type: uint }
  { price: uint, usage-count: uint }
)

;; Intellectual Property Shield
(define-map ip-protection
  { asset-id: uint }
  {
    copyright-timestamp: uint,
    protection-level: uint,
    verified-ownership: bool,
    infringement-reports: uint
  }
)

;; Fair Use Oracle Data
(define-map fair-use-assessments
  { original-asset: uint, derivative-asset: uint }
  {
    similarity-score: uint,
    fair-use-approved: bool,
    assessment-timestamp: uint
  }
)

;; Dynamic Attribution Vectors
(define-map attribution-vectors
  { asset-id: uint }
  {
    market-performance: uint,
    contribution-score: uint,
    influence-factor: uint,
    revenue-multiplier: uint
  }
)

;; Admin Functions
(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-fee u1000) ERR-INVALID-PERCENTAGE)
    (ok (var-set platform-fee-percentage new-fee))
  )
)

(define-public (update-oracle-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= new-threshold u1) (<= new-threshold u100)) ERR-INVALID-PERCENTAGE)
    (ok (var-set oracle-threshold new-threshold))
  )
)

;; Core Creative Asset Functions
(define-public (mint-evolving-asset 
  (title (string-ascii 100))
  (provenance-dna (buff 32))
  (parent-asset (optional uint))
  (license-types uint)
  (inspiration-sources (list 10 uint))
)
  (let
    (
      (asset-id (var-get next-asset-id))
      (creative-distance (calculate-creative-distance parent-asset))
    )
    (asserts! (<= (len title) u100) ERR-INVALID-CONTRIBUTOR)
    (asserts! (validate-license-types license-types) ERR-INVALID-LICENSE-TYPE)
    
    ;; Validate parent asset exists if specified
    (match parent-asset
      parent-id (asserts! (is-some (map-get? creative-assets { asset-id: parent-id })) ERR-PARENT-NOT-FOUND)
      true
    )
    
    ;; Create the creative asset
    (map-set creative-assets
      { asset-id: asset-id }
      {
        creator: tx-sender,
        title: title,
        provenance-dna: provenance-dna,
        parent-asset: parent-asset,
        creative-distance: creative-distance,
        total-revenue: u0,
        creation-timestamp: burn-block-height,
        license-types: license-types,
        is-collaborative: false,
        derivative-count: u0
      }
    )
    
    ;; Set up provenance chain
    (map-set provenance-chains
      { asset-id: asset-id }
      {
        inspiration-sources: inspiration-sources,
        contributor-weights: (list u100),
        genealogy-depth: creative-distance,
        attribution-vector: (generate-attribution-vector asset-id)
      }
    )
    
    ;; Initial revenue split (100% to creator)
    (map-set revenue-splits
      { asset-id: asset-id, beneficiary: tx-sender }
      { percentage: u10000, total-earned: u0 }
    )
    
    ;; Initialize IP protection
    (map-set ip-protection
      { asset-id: asset-id }
      {
        copyright-timestamp: burn-block-height,
        protection-level: u1,
        verified-ownership: true,
        infringement-reports: u0
      }
    )
    
    ;; Update parent's derivative count
    (match parent-asset
      parent-id (update-derivative-count parent-id)
      true
    )
    
    (var-set next-asset-id (+ asset-id u1))
    (ok asset-id)
  )
)

(define-public (create-derivative-asset
  (parent-asset-id uint)
  (title (string-ascii 100))
  (provenance-dna (buff 32))
  (contribution-percentage uint)
)
  (let
    (
      (parent-asset (unwrap! (map-get? creative-assets { asset-id: parent-asset-id }) ERR-ASSET-NOT-FOUND))
      (asset-id (var-get next-asset-id))
    )
    (asserts! (and (>= contribution-percentage u1) (<= contribution-percentage u100)) ERR-INVALID-PERCENTAGE)
    
    ;; Check fair use oracle
    (asserts! (passes-fair-use-check parent-asset-id provenance-dna) ERR-ORACLE-VIOLATION)
    
    ;; Mint derivative asset
    (try! (mint-evolving-asset 
      title 
      provenance-dna 
      (some parent-asset-id) 
      (get license-types parent-asset)
      (list parent-asset-id)
    ))
    
    ;; Set up revenue splits for derivative
    (try! (setup-derivative-revenue-splits asset-id parent-asset-id contribution-percentage))
    
    (ok asset-id)
  )
)

(define-public (purchase-license
  (asset-id uint)
  (license-type uint)
  (payment uint)
)
  (let
    (
      (asset (unwrap! (map-get? creative-assets { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
      (license-price (unwrap! (map-get? license-pricing { asset-id: asset-id, license-type: license-type }) ERR-INVALID-LICENSE-TYPE))
    )
    (asserts! (>= payment (get price license-price)) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (validate-license-availability asset license-type) ERR-INVALID-LICENSE-TYPE)
    
    ;; Process payment and distribute revenue
    (try! (stx-transfer? payment tx-sender (get creator asset)))
    (try! (distribute-revenue asset-id payment))
    
    ;; Update license usage count
    (map-set license-pricing
      { asset-id: asset-id, license-type: license-type }
      {
        price: (get price license-price),
        usage-count: (+ (get usage-count license-price) u1)
      }
    )
    
    (ok true)
  )
)

(define-public (create-collaboration-pool
  (contributors (list 20 principal))
  (percentages (list 20 uint))
  (deadline uint)
)
  (let
    ((pool-id (var-get next-asset-id)))
    (asserts! (is-eq (len contributors) (len percentages)) ERR-INVALID-CONTRIBUTOR)
    (asserts! (is-eq (fold + percentages u0) u10000) ERR-INVALID-PERCENTAGE)
    (asserts! (> deadline burn-block-height) ERR-INVALID-CONTRIBUTOR)
    
    (map-set collaboration-pools
      { pool-id: pool-id }
      {
        contributors: contributors,
        contribution-percentages: percentages,
        pool-balance: u0,
        is-active: true,
        creation-deadline: deadline
      }
    )
    
    (ok pool-id)
  )
)

(define-public (report-infringement (original-asset uint) (infringing-asset uint))
  (let
    (
      (original (unwrap! (map-get? creative-assets { asset-id: original-asset }) ERR-ASSET-NOT-FOUND))
      (ip-data (unwrap! (map-get? ip-protection { asset-id: original-asset }) ERR-ASSET-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get creator original)) ERR-NOT-AUTHORIZED)
    
    (map-set ip-protection
      { asset-id: original-asset }
      {
        copyright-timestamp: (get copyright-timestamp ip-data),
        protection-level: (get protection-level ip-data),
        verified-ownership: (get verified-ownership ip-data),
        infringement-reports: (+ (get infringement-reports ip-data) u1)
      }
    )
    
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-asset-details (asset-id uint))
  (map-get? creative-assets { asset-id: asset-id })
)

(define-read-only (get-