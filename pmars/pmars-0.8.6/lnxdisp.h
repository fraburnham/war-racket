/* portable MARS, corewar simulator with ICWS'94 extensions
 * Copyright (c) 1993-95 by Albert Ma, Na'ndor Sieben, Stefan Strack,
 * Mintardjo Wangsaw and Martin Maierhofer
 *
 * lnxdisp.h: user interface for Linux svga graphics displays
 *            mostly stolen from grxdisp.c, uidisp.c, xgraphio.c
 * $Id: lnxdisp.h,v 1.3 1995/07/13 17:12:56 stst Exp stst $
 */

#ifdef LINUXGRAPHX

#ifndef LNXDISP_H
#define LNXDISP_H

#include <vga.h>
#include <vgagl.h>

/* variable declaration */
extern int      x, y;		/* screen position of current address */
extern int      col;		/* screen color of current address */
extern int      processRatio;	/* ratio pixel of process meter to process */
extern int      splY[];		/* locations of the process meters */
extern int      colors[];	/* colors of the warriors */
extern int      datcolors[];	/* deatch colors of the warriors */
extern int      clearColor;	/* the color to erase with, ie. BLACK */

/* extern prototype declaration */
#ifdef NEW_STYLE
extern void     svga_open_graphics(void);
extern void     svga_display_cycle(void);
extern void     svga_display_clear(void);
extern int      xkoord(int addr);
extern int      ykoord(int addr);
extern void     findplace(int addr);
extern void     gl_rect(int x, int y, int xx, int yy, int c);
#else
extern void     svga_open_graphics();
extern void     svga_display_cycle();
extern void     svga_display_clear();
extern int      xkoord();
extern int      ykoord();
extern void     findplace();
extern void     gl_rect();
#endif

/* functional macros needed by sim.c */
#define display_die(warnum)
#define display_push(val)

#define display_init()  svga_open_graphics()
#define display_clear() svga_display_clear()
#define display_close() svga_display_close(WAIT)
#define display_cycle() svga_display_cycle()

#define svga_display_read(addr) \
  gl_setpixel(xkoord(addr), ykoord(addr), colors[W-warrior])

#define svga_display_dec(addr) \
do { \
  findplace(addr); \
  gl_setpixel(x  , y, col); \
  gl_setpixel(x+1, y, col); \
} while (0)

#define svga_display_inc(addr) \
do { \
  findplace(addr); \
  gl_setpixel(x, y  , col); \
  gl_setpixel(x, y+1, col); \
} while(0)

#define svga_display_write(addr) \
do { \
  findplace(addr); \
  gl_setpixel(x+1, y, col); \
  gl_setpixel(x  , y+1, col); \
} while(0)

#define svga_display_exec(addr) \
do { \
    gl_rect(xkoord(addr), ykoord(addr), \
      xkoord(addr)+1, ykoord(addr)+1, colors[W-warrior]); \
} while(0)


#define svga_display_spl(warrior, tasks) \
  gl_setpixel(tasks/processRatio, splY[warrior], colors[warrior])

#define svga_display_dat(addr, warNum, tasks) \
  do {\
    if (displayLevel>  0) { \
        gl_fillbox(xkoord(addr), ykoord(addr), 1, 1, datcolors[warNum]); \
    } \
    gl_setpixel(tasks/processRatio, splY[warNum], clearColor); \
  } while(0)

#define display_read(addr) do { if (displayLevel > 3)\
  svga_display_read(addr); } while(0)
#define display_write(addr) do { if (displayLevel > 1)\
  svga_display_write(addr); } while(0)
#define display_dec(addr) do { if (displayLevel > 2)\
  svga_display_dec(addr); } while (0)
#define display_inc(addr)  do { if (displayLevel > 2)\
  svga_display_inc(addr); } while(0)
#define display_exec(addr) do { if (displayLevel > 0)\
  svga_display_exec(addr); } while(0)
#define display_spl(warrior, tasks) svga_display_spl(warrior, tasks)
#define display_dat(addr, warnum, tasks) svga_display_dat(addr, warnum, tasks)

#endif				/* LNXDISP_H */
#endif				/* defined LINUXGRAPHX */
