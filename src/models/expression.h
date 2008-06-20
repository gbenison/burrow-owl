
#ifndef EXPRESSION_H
#define EXPRESSION_H

typedef enum
{
	XPR_NUMBER,
	XPR_ADD,
	XPR_SUB,
	XPR_MUL,
	XPR_DIV,
	XPR_EXP,
	XPR_UNARY_MINUS,
	XPR_SQR
} xpr_code_t;

typedef struct xpr_struct xpr_t;

/* constructors */
xpr_t* expression_make_number      (double mean, double stddev);
xpr_t* expression_make_sum         (xpr_t* A, xpr_t* B);
xpr_t* expression_make_product     (xpr_t* A, xpr_t* B);
xpr_t* expression_make_ratio       (xpr_t* A, xpr_t* B);
xpr_t* expression_make_exponential (xpr_t* exponent);
xpr_t* expression_make_negative    (xpr_t* xpr);

/* accessors */
double expression_eval         (xpr_t* expr);
double expression_mean         (xpr_t* self);
double expression_stddev       (xpr_t* self);
double expression_prior_mean   (xpr_t* self);
double expression_prior_stddev (xpr_t* self);

void   expression_assert_number    (xpr_t* self);
void   expression_set_value        (xpr_t* self, double value);
void   expression_set_prior_mean   (xpr_t* self, double value);
void   expression_set_prior_stddev (xpr_t* self, double value);
void   expression_nudge_value      (xpr_t* self, double value);
void   expression_revert           (xpr_t* self);
void   expression_accumulate       (xpr_t* self);
void   expression_stats_init       (xpr_t* self);
double expression_probability      (xpr_t* self);


#endif /* EXPRESSION_H */


