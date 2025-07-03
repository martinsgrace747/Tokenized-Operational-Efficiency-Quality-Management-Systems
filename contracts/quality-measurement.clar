;; Quality Measurement Contract
;; Handles quality metric collection and validation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-INVALID-MEASUREMENT (err u201))
(define-constant ERR-NOT-FOUND (err u202))
(define-constant ERR-ALREADY-EXISTS (err u203))

;; Data Variables
(define-data-var next-measurement-id uint u1)
(define-data-var measurement-threshold uint u50)

;; Data Maps
(define-map quality-measurements
  { measurement-id: uint }
  {
    coordinator-id: uint,
    metric-type: (string-ascii 30),
    value: uint,
    target-value: uint,
    measurement-date: uint,
    validation-status: (string-ascii 20),
    validator-count: uint,
    block-height: uint
  }
)

(define-map measurement-validations
  { measurement-id: uint, validator: principal }
  {
    validation-score: uint,
    validation-date: uint,
    comments: (string-ascii 200)
  }
)

(define-map metric-definitions
  { metric-type: (string-ascii 30) }
  {
    description: (string-ascii 100),
    unit: (string-ascii 20),
    min-value: uint,
    max-value: uint,
    weight: uint,
    active: bool
  }
)

(define-map coordinator-metrics
  { coordinator-id: uint, metric-type: (string-ascii 30), period: uint }
  {
    total-measurements: uint,
    average-value: uint,
    best-value: uint,
    trend-direction: (string-ascii 10)
  }
)

;; Public Functions

;; Define a new metric type
(define-public (define-metric
  (metric-type (string-ascii 30))
  (description (string-ascii 100))
  (unit (string-ascii 20))
  (min-value uint)
  (max-value uint)
  (weight uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (is-none (map-get? metric-definitions { metric-type: metric-type })) ERR-ALREADY-EXISTS)
    (asserts! (< min-value max-value) ERR-INVALID-MEASUREMENT)

    (map-set metric-definitions
      { metric-type: metric-type }
      {
        description: description,
        unit: unit,
        min-value: min-value,
        max-value: max-value,
        weight: weight,
        active: true
      }
    )
    (ok true)
  )
)

;; Submit a quality measurement
(define-public (submit-measurement
  (coordinator-id uint)
  (metric-type (string-ascii 30))
  (value uint)
  (target-value uint))
  (let
    (
      (measurement-id (var-get next-measurement-id))
      (metric-def (unwrap! (map-get? metric-definitions { metric-type: metric-type }) ERR-NOT-FOUND))
    )
    (asserts! (get active metric-def) ERR-INVALID-MEASUREMENT)
    (asserts! (>= value (get min-value metric-def)) ERR-INVALID-MEASUREMENT)
    (asserts! (<= value (get max-value metric-def)) ERR-INVALID-MEASUREMENT)

    (map-set quality-measurements
      { measurement-id: measurement-id }
      {
        coordinator-id: coordinator-id,
        metric-type: metric-type,
        value: value,
        target-value: target-value,
        measurement-date: block-height,
        validation-status: "pending",
        validator-count: u0,
        block-height: block-height
      }
    )

    (var-set next-measurement-id (+ measurement-id u1))
    (ok measurement-id)
  )
)

;; Validate a measurement
(define-public (validate-measurement
  (measurement-id uint)
  (validation-score uint)
  (comments (string-ascii 200)))
  (let
    (
      (measurement (unwrap! (map-get? quality-measurements { measurement-id: measurement-id }) ERR-NOT-FOUND))
      (validator tx-sender)
    )
    (asserts! (<= validation-score u100) ERR-INVALID-MEASUREMENT)
    (asserts! (is-none (map-get? measurement-validations { measurement-id: measurement-id, validator: validator })) ERR-ALREADY-EXISTS)

    (map-set measurement-validations
      { measurement-id: measurement-id, validator: validator }
      {
        validation-score: validation-score,
        validation-date: block-height,
        comments: comments
      }
    )

    (let
      (
        (new-validator-count (+ (get validator-count measurement) u1))
        (new-status (if (>= new-validator-count u3) "validated" "pending"))
      )
      (map-set quality-measurements
        { measurement-id: measurement-id }
        (merge measurement
          {
            validator-count: new-validator-count,
            validation-status: new-status
          }
        )
      )
    )
    (ok true)
  )
)

;; Update coordinator metrics summary
(define-public (update-coordinator-metrics
  (coordinator-id uint)
  (metric-type (string-ascii 30))
  (period uint)
  (total-measurements uint)
  (average-value uint)
  (best-value uint)
  (trend-direction (string-ascii 10)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)

    (map-set coordinator-metrics
      { coordinator-id: coordinator-id, metric-type: metric-type, period: period }
      {
        total-measurements: total-measurements,
        average-value: average-value,
        best-value: best-value,
        trend-direction: trend-direction
      }
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get measurement details
(define-read-only (get-measurement (measurement-id uint))
  (map-get? quality-measurements { measurement-id: measurement-id })
)

;; Get measurement validation
(define-read-only (get-measurement-validation (measurement-id uint) (validator principal))
  (map-get? measurement-validations { measurement-id: measurement-id, validator: validator })
)

;; Get metric definition
(define-read-only (get-metric-definition (metric-type (string-ascii 30)))
  (map-get? metric-definitions { metric-type: metric-type })
)

;; Get coordinator metrics
(define-read-only (get-coordinator-metrics (coordinator-id uint) (metric-type (string-ascii 30)) (period uint))
  (map-get? coordinator-metrics { coordinator-id: coordinator-id, metric-type: metric-type, period: period })
)

;; Calculate quality score for a measurement
(define-read-only (calculate-quality-score (measurement-id uint))
  (match (map-get? quality-measurements { measurement-id: measurement-id })
    measurement
    (let
      (
        (value (get value measurement))
        (target (get target-value measurement))
        (variance (if (> value target) (- value target) (- target value)))
        (score (if (<= variance u10) u100 (- u100 (* variance u5))))
      )
      (some (if (< score u0) u0 score))
    )
    none
  )
)

;; Get total measurements count
(define-read-only (get-total-measurements)
  (- (var-get next-measurement-id) u1)
)
