;; Decentralized Crowdfunding Platform

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-deadline-passed (err u104))
(define-constant err-goal-not-reached (err u105))
(define-constant err-already-claimed (err u106))
(define-constant  err-transfer-failed (err u107))

;; Data Maps
(define-map campaigns
  { campaign-id: uint }
  {
    owner: principal,
    goal: uint,
    raised: uint,
    deadline: uint,
    claimed: bool
  }
)

(define-map contributions
  { campaign-id: uint, contributor: principal }
  { amount: uint }
)

(define-map campaign-descriptions
  { campaign-id: uint }
  { description: (string-utf8 500) })

(define-private (current-time)
  (unwrap-panic (get-block-info? time u0)))


;; Variables
(define-data-var campaign-nonce uint u0)


  ;; Read-only Functions
(define-read-only (get-campaign-details (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id }))

(define-read-only (get-contribution (campaign-id uint) (contributor principal))
  (map-get? contributions { campaign-id: campaign-id, contributor: contributor }))

(define-read-only (get-total-campaigns)
  (var-get campaign-nonce))
  

(define-read-only (is-campaign-successful (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (and 
              (>= (get raised campaign) (get goal campaign))
              (>= (current-time) (get deadline campaign)))
    false))

(define-read-only (get-campaign-time-left (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (let ((time-left (- (get deadline campaign) (current-time))))
              (if (< (current-time) (get deadline campaign))
                (ok time-left)
                (ok u0)))
    (err err-not-found)))

(define-read-only (get-campaign-progress (campaign-id uint))
  (match (get-campaign-details campaign-id)
    campaign (let ((progress (* (/ (get raised campaign) (get goal campaign)) u100)))
              (ok progress))
    (err err-not-found)))

;; Read-only function to get campaign description
(define-read-only (get-campaign-description (campaign-id uint))
  (map-get? campaign-descriptions { campaign-id: campaign-id }))

;; Public Functions
(define-public (create-campaign (goal uint) (deadline uint))
  (let
    (
      (campaign-id (var-get campaign-nonce))
    )
    (asserts! (> goal u0) (err err-invalid-amount))
    (asserts! (> deadline (current-time)) (err err-deadline-passed))
    (map-insert campaigns
      { campaign-id: campaign-id }
      {
        owner: tx-sender,
        goal: goal,
        raised: u0,
        deadline: deadline,
        claimed: false
      }
    )
    (var-set campaign-nonce (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (contribute (campaign-id uint) (amount uint))
  (let
    (
      (campaign (unwrap! (map-get? campaigns { campaign-id: campaign-id }) (err err-not-found)))
      (current-raised (get raised campaign))
      (new-raised (+ current-raised amount))
    )
    (asserts! (< (current-time) (get deadline campaign)) (err err-deadline-passed))
    (asserts! (> amount u0) (err err-invalid-amount))
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success
        (begin
          (map-set campaigns
            { campaign-id: campaign-id }
            (merge campaign { raised: new-raised })
          )
          (map-set contributions
            { campaign-id: campaign-id, contributor: tx-sender }
            { amount: (+ amount (default-to u0 (get amount (map-get? contributions { campaign-id: campaign-id, contributor: tx-sender })))) }
          )
          (ok true)
        )
      error (err err-transfer-failed)
    )
  )
)



