
; generate types derived from models.

(use-modules (ice-9 regex)
	     (srfi srfi-1))

(define header-file (open-output-file "model-subtypes-gen.h"))
(define body-file (open-output-file "model-subtypes-gen.c"))

(define (string-substitute-all pattern substitute str)
  (define rx (make-regexp pattern))
  (let loop ((result str))
    (let ((match (regexp-exec rx result)))
      (if match
	  (loop (regexp-substitute #f match 'pre substitute 'post))
	  result))))

(define (substitute-class-name name str)
  (string-substitute-all "@Name@" (as-studly-caps name)
    (string-substitute-all "@name@" (as-underscored name)
      (string-substitute-all "@NAME@" (as-all-caps name) str))))

(define str->canonical
  (let* ((char-set:canonical (char-set-union (char-set #\_) char-set:letter+digit))
	 (char-set:other (char-set-complement char-set:canonical)))
    (lambda (str)
      (let loop ((str str))
	(let ((idx (string-index str char-set:other)))
	  (if idx 
	      (begin (string-set! str idx #\_)
		     (loop str))
	      str))))))

(define (strip-underscores str)
  (list->string (filter (lambda (x)(not (equal? x #\_)))(string->list str))))

(define (as-studly-caps str)
  (strip-underscores (string-titlecase (str->canonical str))))

(define (as-underscored str)
  (string-downcase (str->canonical str)))

(define (as-all-caps str)
  (string-upcase (str->canonical str)))

(define (generate-derived-type name)
  (display (substitute-class-name name
"
#define HOS_TYPE_MODEL_@NAME@              (hos_model_@name@_get_type())
#define HOS_MODEL_@NAME@(obj)              (G_TYPE_CHECK_INSTANCE_CAST ((obj), HOS_TYPE_MODEL_@NAME@, HosModel@Name@))
#define HOS_MODEL_@NAME@_CLASS(klass)      (G_TYPE_CHECK_CLASS_CAST ((klass), HOS_TYPE_MODEL_@NAME@, HosModel@Name@Class))
#define HOS_IS_MODEL_@NAME@(obj)           (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HOS_TYPE_MODEL_@NAME@))
#define HOS_IS_MODEL_@NAME@_CLASS(klass)   (G_TYPE_CHECK_CLASS_TYPE ((klass), HOS_TYPE_MODEL_@NAME@))
#define HOS_MODEL_@NAME@_GET_CLASS(obj)    (G_TYPE_INSTANCE_GET_CLASS ((obj), HOS_TYPE_MODEL_@NAME@, HosModel@Name@Class))

typedef struct _HosModel@Name@       HosModel@Name@;
typedef struct _HosModel@Name@Class  HosModel@Name@Class;

GType hos_model_@name@_get_type(void);

")
	   header-file)
  (display (substitute-class-name name
"
G_DEFINE_TYPE (HosModel@Name@, hos_model_@name@, HOS_TYPE_MODEL)

static void    model_@name@_iterator_fill (model_iterator_t *self, gdouble *dest);
static void    model_@name@_iterator_init (model_iterator_t *self, gdouble *orig, gdouble *delta, guint *np);
static void    model_@name@_iterator_free (model_iterator_t *self);

static void
hos_model_@name@_class_init(HosModel@Name@Class *klass)
{
  GObjectClass  *gobject_class = G_OBJECT_CLASS(klass);
  HosModelClass *model_class   = HOS_MODEL_CLASS(klass);

  model_class->iterator_fill = model_@name@_iterator_fill;
  model_class->iterator_init = model_@name@_iterator_init;
  model_class->iterator_free = model_@name@_iterator_free;
}

static void
hos_model_@name@_init(HosModel@Name@ *self)
{
 HosModel *model = HOS_MODEL(self);
 model->ndim = HOS_IS_MODEL_DIMENSION(self) ? 1 : 0;
}

")
	   body-file)
  'ok)

(generate-derived-type "sum")
(generate-derived-type "product")
(generate-derived-type "gaussian")
(generate-derived-type "dimension")
(generate-derived-type "noise")



