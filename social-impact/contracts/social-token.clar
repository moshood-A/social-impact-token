;; Social Impact Token (SIT) Smart Contract
;; A comprehensive token system for tracking and rewarding social impact initiatives

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_INVALID_RECIPIENT (err u103))
(define-constant ERR_PROJECT_NOT_FOUND (err u104))
(define-constant ERR_ALREADY_EXISTS (err u105))
(define-constant ERR_INVALID_PARAMETER (err u106))

;; Token Properties
(define-fungible-token social-impact-token)
(define-data-var token-name (string-ascii 32) "Social Impact Token")
(define-data-var token-symbol (string-ascii 10) "SIT")
(define-data-var token-decimals uint u6)
(define-data-var total-supply uint u0)

;; Admin and Governance
(define-data-var contract-owner principal CONTRACT_OWNER)
(define-map authorized-minters principal bool)
(define-map project-validators principal bool)

;; Impact Projects Tracking
(define-map impact-projects 
  { project-id: uint }
  {
    creator: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    target-amount: uint,
    raised-amount: uint,
    impact-score: uint,
    is-active: bool,
    created-at: uint
  })

(define-data-var next-project-id uint u1)

;; Impact Metrics
(define-map user-impact-scores principal uint)
(define-map project-contributions { project-id: uint, contributor: principal } uint)
(define-map impact-multipliers principal uint)

;; Events for transparency
(define-map project-milestones 
  { project-id: uint, milestone-id: uint }
  {
    description: (string-ascii 100),
    impact-increase: uint,
    verified-by: principal,
    timestamp: uint
  })

;; Read-only functions
(define-read-only (get-name)
  (ok (var-get token-name)))

(define-read-only (get-symbol)
  (ok (var-get token-symbol)))

(define-read-only (get-decimals)
  (ok (var-get token-decimals)))

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance social-impact-token who)))

(define-read-only (get-total-supply)
  (ok (ft-get-supply social-impact-token)))

(define-read-only (get-contract-owner)
  (ok (var-get contract-owner)))

(define-read-only (is-authorized-minter (who principal))
  (default-to false (map-get? authorized-minters who)))

(define-read-only (get-project-info (project-id uint))
  (map-get? impact-projects { project-id: project-id }))

(define-read-only (get-user-impact-score (user principal))
  (default-to u0 (map-get? user-impact-scores user)))

(define-read-only (get-project-contribution (project-id uint) (contributor principal))
  (default-to u0 (map-get? project-contributions { project-id: project-id, contributor: contributor })))

(define-read-only (get-impact-multiplier (user principal))
  (default-to u100 (map-get? impact-multipliers user)))

;; Administrative functions
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-standard new-owner) ERR_INVALID_PARAMETER)
    (var-set contract-owner new-owner)
    (ok true)))

(define-public (add-authorized-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-standard minter) ERR_INVALID_PARAMETER)
    (asserts! (not (is-authorized-minter minter)) ERR_ALREADY_EXISTS)
    (map-set authorized-minters minter true)
    (ok true)))

(define-public (remove-authorized-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-standard minter) ERR_INVALID_PARAMETER)
    (asserts! (is-authorized-minter minter) ERR_PROJECT_NOT_FOUND)
    (map-delete authorized-minters minter)
    (ok true)))

(define-public (add-project-validator (validator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-standard validator) ERR_INVALID_PARAMETER)
    (asserts! (not (default-to false (map-get? project-validators validator))) ERR_ALREADY_EXISTS)
    (map-set project-validators validator true)
    (ok true)))

;; Core token functions
(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-standard recipient) ERR_INVALID_RECIPIENT)
    (asserts! (is-eq tx-sender sender) ERR_UNAUTHORIZED)
    (try! (ft-transfer? social-impact-token amount sender recipient))
    (print { action: "transfer", amount: amount, sender: sender, recipient: recipient, memo: memo })
    (ok true)))

(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (or (is-eq tx-sender (var-get contract-owner)) 
                  (is-authorized-minter tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-standard recipient) ERR_INVALID_RECIPIENT)
    (try! (ft-mint? social-impact-token amount recipient))
    (print { action: "mint", amount: amount, recipient: recipient })
    (ok true)))

(define-public (burn (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-burn? social-impact-token amount tx-sender))
    (print { action: "burn", amount: amount, sender: tx-sender })
    (ok true)))

;; Impact project functions
(define-public (create-impact-project (name (string-ascii 50)) (description (string-ascii 200)) (target-amount uint))
  (let ((project-id (var-get next-project-id)))
    (asserts! (> target-amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMETER)
    (asserts! (> (len description) u0) ERR_INVALID_PARAMETER)
    (asserts! (<= (len description) u200) ERR_INVALID_PARAMETER)
    (map-set impact-projects 
      { project-id: project-id }
      {
        creator: tx-sender,
        name: name,
        description: description,
        target-amount: target-amount,
        raised-amount: u0,
        impact-score: u0,
        is-active: true,
        created-at: stacks-block-height
      })
    (var-set next-project-id (+ project-id u1))
    (print { action: "project-created", project-id: project-id, creator: tx-sender })
    (ok project-id)))

(define-public (contribute-to-project (project-id uint) (amount uint))
  (let ((project (unwrap! (get-project-info project-id) ERR_PROJECT_NOT_FOUND)))
    (let ((current-contribution (get-project-contribution project-id tx-sender))
          (multiplier (get-impact-multiplier tx-sender))
          (impact-bonus (/ (* amount multiplier) u100)))
      (asserts! (> amount u0) ERR_INVALID_AMOUNT)
      (asserts! (get is-active project) ERR_INVALID_PARAMETER)
      (try! (ft-transfer? social-impact-token amount tx-sender (get creator project)))
      
      ;; Update project contribution
      (map-set project-contributions 
        { project-id: project-id, contributor: tx-sender } 
        (+ current-contribution amount))
      
      ;; Update project raised amount
      (map-set impact-projects 
        { project-id: project-id }
        (merge project { raised-amount: (+ (get raised-amount project) amount) }))
      
      ;; Award impact score
      (unwrap-panic (update-impact-score tx-sender impact-bonus))
      (print { action: "contribution", project-id: project-id, contributor: tx-sender, amount: amount })
      (ok true))))

(define-public (add-project-milestone (project-id uint) (milestone-id uint) (description (string-ascii 100)) (impact-increase uint))
  (let ((project (unwrap! (get-project-info project-id) ERR_PROJECT_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender (get creator project))
                  (default-to false (map-get? project-validators tx-sender))) ERR_UNAUTHORIZED)
    (asserts! (> milestone-id u0) ERR_INVALID_PARAMETER)
    (asserts! (<= milestone-id u1000) ERR_INVALID_PARAMETER)
    (asserts! (> (len description) u0) ERR_INVALID_PARAMETER)
    (asserts! (<= (len description) u100) ERR_INVALID_PARAMETER)
    (asserts! (<= impact-increase u10000) ERR_INVALID_PARAMETER)
    (asserts! (is-none (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })) ERR_ALREADY_EXISTS)
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      {
        description: description,
        impact-increase: impact-increase,
        verified-by: tx-sender,
        timestamp: stacks-block-height
      })
    
    ;; Update project impact score
    (map-set impact-projects 
      { project-id: project-id }
      (merge project { impact-score: (+ (get impact-score project) impact-increase) }))
    
    (print { action: "milestone-added", project-id: project-id, milestone-id: milestone-id })
    (ok true)))

(define-public (set-impact-multiplier (user principal) (multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-standard user) ERR_INVALID_PARAMETER)
    (asserts! (and (>= multiplier u50) (<= multiplier u200)) ERR_INVALID_PARAMETER)
    (map-set impact-multipliers user multiplier)
    (ok true)))

(define-public (reward-impact-tokens (recipient principal) (amount uint) (reason (string-ascii 50)))
  (begin
    (asserts! (or (is-eq tx-sender (var-get contract-owner)) 
                  (is-authorized-minter tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-standard recipient) ERR_INVALID_PARAMETER)
    (asserts! (> (len reason) u0) ERR_INVALID_PARAMETER)
    (try! (ft-mint? social-impact-token amount recipient))
    (unwrap-panic (update-impact-score recipient (/ amount u10)))
    (print { action: "impact-reward", recipient: recipient, amount: amount, reason: reason })
    (ok true)))

(define-public (deactivate-project (project-id uint))
  (let ((project (unwrap! (get-project-info project-id) ERR_PROJECT_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender (get creator project))
                  (is-eq tx-sender (var-get contract-owner))) ERR_UNAUTHORIZED)
    (asserts! (get is-active project) ERR_INVALID_PARAMETER)
    (map-set impact-projects 
      { project-id: project-id }
      (merge project { is-active: false }))
    (print { action: "project-deactivated", project-id: project-id })
    (ok true)))

(define-public (emergency-pause-project (project-id uint))
  (let ((project (unwrap! (get-project-info project-id) ERR_PROJECT_NOT_FOUND)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (< project-id (var-get next-project-id)) ERR_INVALID_PARAMETER)
    (asserts! (get is-active project) ERR_INVALID_PARAMETER)
    (map-set impact-projects 
      { project-id: project-id }
      (merge project { is-active: false }))
    (print { action: "emergency-pause", project-id: project-id })
    (ok true)))

;; Private helper functions
(define-private (update-impact-score (user principal) (score-increase uint))
  (begin
    (let ((current-score (get-user-impact-score user)))
      (map-set user-impact-scores user (+ current-score score-increase)))
    (ok true)))

;; Initialize contract
(begin
  (try! (ft-mint? social-impact-token u1000000 CONTRACT_OWNER))
  (map-set authorized-minters CONTRACT_OWNER true)
  (map-set project-validators CONTRACT_OWNER true)
  (print { action: "contract-initialized", owner: CONTRACT_OWNER }))