/* -----------------------------------------------------------------------
   Header file for Linux PIC84 programmer
   
   Copyright 1994-1998 by Brian C. Lane
   ----------------------------------------------------------------------- */
#include <stdio.h>
#include <curses.h>
#include <unistd.h>

#define flushall() fflush(NULL)

#define NUM_PORTS       10             /* Number of parallel ports */
#define MAXPATHLEN      1024           /* Maximum path length */

#define	PIC16C84	0

#define OPT_QUIET       0x0001
#define OPT_VERIFY      0x0002
#define OPT_DEBUG       0x0004

/* How to genericly handle the ports, lines, and polarities? */
struct _pconfig
{
  int	port;				/* /dev/lpX		 	*/
  int	colors;				/* Color support 1 or 0		*/
  int	PIC_type;			/* Default PIC type		*
  					 * 0 = PIC16C84			*
  					 * Others to be defined		*/
  unsigned int	CFaddr;			/* Config Fuses Address 0x2007	*/
  unsigned int	IDaddr;			/* ID Memory address 0x2000	*/
  unsigned int	EEaddr;			/* EEPROM address 0x2100	*/
  int	VPP_pin;			/* Pin for VPP control		*/
  int	VPP_pol;			/* Polarity 0=inverted 1=normal	*/
  int	VDD_pin;			/* Pin for Vdd control		*/
  int	VDD_pol;			/* Polarity 0=inverted 1=normal	*/
  int	CLK_pin;			/* Pin for Clock control	*/
  int	CLK_pol;			/* Polarity 0=inverted 1=normal	*/
  int	DATAO_pin;			/* Pin for Data to PIC control	*/
  int	DATAO_pol;			/* Polarity 0=inverted 1=normal	*/
  int	DATAI_pin;			/* Pin for Data from PIC control*/
  int	DATAI_pol;			/* Polarity 0=inverted 1=normal	*/
};

/* Structure to hold all the data about the currently loaded processor	*/
#define MAXPICSIZE 65535

struct _pic
{
  unsigned int	mem[MAXPICSIZE];	/* up to 16 bit words		*/
};


int init_port();
int verify_pic( WINDOW * );
int program_pic( WINDOW * );
int read_pic( WINDOW * );
int readihex( FILE *, unsigned int *, int *, int * );
int writeihex16( FILE *, unsigned int *, int, int );
int writeiend16( FILE * );
void attr_clear(WINDOW *, int, int, chtype);
void show_err( char *fmt, ... );
int inbit();
int vppon();
int vppoff();
int vddon();
int vddoff();
int clkhi();
int clklo();
int outhi();
int outlo();
void init_pic();
void init_lp_table();
