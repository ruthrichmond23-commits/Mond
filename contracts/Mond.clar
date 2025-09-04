;; winning-mini-rfp.clar
;; Brief & unique Clarity RFP contract for Google Clarity Web3

(clarity-version 2)

(define-data-var rfp-counter uint u0)
(define-map rfps ((id uint))
  ((owner principal) (commit-end uint) (reveal-end uint) (winner (optional principal))))
(define-map commits ((id uint) (vendor principal)) ((h (buff 32))))
(define-map reveals ((id uint) (vendor principal)) ((proposal (string-utf8 40))))

;; Create RFP
(define-public (create-rfp (commit-end uint) (reveal-end uint))
  (let ((id (+ (var-get rfp-counter) u1)))
    (asserts! (> reveal-end commit-end) (err u100))
    (map-set rfps { id: id } { owner: tx-sender, commit-end: commit-end, reveal-end: reveal-end, winner: none })
    (var-set rfp-counter id)
    (ok id)))

;; Commit hashed proposal
(define-public (commit (id uint) (h (buff 32)))
  (begin
    (asserts! (<= block-height (get commit-end (unwrap! (map-get? rfps { id: id }) (err u101)))) (err u102))
    (map-set commits { id: id, vendor: tx-sender } { h: h })
    (ok true)))

;; Reveal proposal with salt
(define-public (reveal (id uint) (proposal (string-utf8 40)) (salt (buff 32)))
  (let ((c (map-get? commits { id: id, vendor: tx-sender })))
    (match c
      somec (asserts! (is-eq (get h somec) (sha256 (concat (utf8-to-bytes proposal) salt))) (err u103))
             (map-set reveals { id: id, vendor: tx-sender } { proposal: proposal })
             (ok true))
      none (err u104))))

;; Finalize winner
(define-public (finalize (id uint) (winner principal))
  (let ((r (map-get? rfps { id: id })))
    (match r
      some (begin
              (asserts! (is-eq (get owner some) tx-sender) (err u105))
              (asserts! (> block-height (get reveal-end some)) (err u106))
              (map-set rfps { id: id } (merge some { winner: (some winner) }))
              (ok winner))
      none (err u107)))))
