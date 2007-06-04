#ifndef _HAVE_FILEBUF_H
#define _HAVE_FILEBUF_H

typedef struct _filebuf_struct FileBuf;

extern FileBuf *filebuf_new_file(const char*, const int size);
extern void filebuf_peek(FileBuf *fb, void*, const int);
extern void filebuf_seek_set(FileBuf *fb, unsigned long);
extern void filebuf_seek_cur(FileBuf *fb, long);

#endif /* not  _HAVE_FILEBUF_H */
