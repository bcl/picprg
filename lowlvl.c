/* ------------------------------------------------------------------------
   Low Level code for Linux PIC programmer
   
   Copyright 1994-2002 by Brian C. Lane
   All Rights Reserved

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

   ---------------------------------------------------------------------
   This is the way Russ has his programmer connected
   to the parallel port.

   DB25 connections
    1 = !C0    = !PGM
    2 = D0     = CLK out
    3 = D1     = DATI (to PIC)
    11 = !S7   = DATO (from PIC)
    14 = !C1   = MODE control 0=programming  1=normal
    25 = GND

   David Tait's Parallel port PIC programmer connections:
   May or may not be inverted!
      
   2 = D0      = Data out
   3 = D1      = Clock out
   4 = D2      = +5v Power control
   5 = D3      = +Vpp/!MCLR control
   10= !ACK    = Data in

   ------------------------------------------------------------------------
   08/23/2002   Not sure what was wrong, but it works fine today. It also
   bcl          appears that the pconfig.delay setting isn't really needed
                for anything. The waitus() is sufficient for the timing
		needed. Removing it, but leaving the waitus(1) calls in
		inb and outb as suggested by Lee Olsen.

   08/22/2002   Changes to biton and off where ~ was changed to ! was wrong
   bcl          It now reads the PIC. It even erases it to 0x3FFF but when
                I try to program it, all attempts return verify errors.
		Changed all waitus() calls to use pconfig.delay to control
		its delay time. Still won't program.

   08/21/2002   Changed the waitus calls in read_pic() to use pconfig.delay
   bcl          to see if slowing things down will help it work correctly.
   

   08/19/2002   Changed ioperm to start at port, not port+1
   bcl

   08/18/2002   Changing how I address the port. The kernel support for the
   bcl          direct I/O interface has changed and it split up into parts
                that only appear in the kernel source. The only real piece
		of information needed is the base address. Removing the
		lp_struct and LP_B usage. Using lp_table as a list of port
		addresses.
		Changing the names of inb and outb to pp_ so that they do
		not conflict with already existing kernel functions.

		Changing inb and outb to match the current /sys/io.h defines
		I still get warnings about them though.

   08/16/2002   Adding changes from Lee Olsen to read /proc/ioports to get
   bcl          port information and support PCI PIO cards that have high
                io port addresses (0xC400 in his case). Reportedly also
		works on 2.2.x kernels.
		Use iopl(3) instead of ioperm() for large addresses.
		Added waitus(1) calls in inb and outb to make things work,
		and changed ~inb to !inb in inbit()
		Q. about this last change -- what's the diff between ! and ~?

		I don't like using iopl, it isn't specific enough, giving
		access to all ports as far as I can tell. A really bad thing!

   07/05/98 bcl Adding TRUE/FALSE return from verify_pic
                Adding quiet mode (from options & OPT_QUIET)

   11/15/97 bcl Revising this software to work with kernel v2.0.31
   		I seem to have lost my code that lets the user specify
   		the parallel port pin number and state for each control
   		line. I guess I'll have to rewrite that once I get this
   		functioning in its current state. Having trouble with
   		ioperm... Fixed, needed a 1L passed, not a 1. I really
   		hate that.
   		Adding pin definitions.
                Chaninging everything so that it uses the pin_table.
                Pretty cool!

   11/22/94 bcl Thanksgiving Vacation. Should have the time to finish this.

   11/21/94 bcl	I changed over to using the function pointer array. Made
   		everything work 'Really Badly'!
   
   ------------------------------------------------------------------------ */
#include "picprg.h"
#include <unistd.h>
#include <sys/io.h>
#include <fcntl.h>
#include <linux/lp.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <asm/system.h>

extern int options;			/* Options 			*/
static unsigned lp_data[3];		/* Current levels		*/
int port_fd = -1;
int max_lp;

extern struct _pic pic;
extern struct _pconfig pconfig;

/* Table of parallel port addresses, maximum of 6 supported */
unsigned int lp_table[NUM_PORTS];

/* 
   Base + 0   Data I/O Address    TTL Output Only/(Tristate->Input)
   ---------------------------------------------------------------------
   Bit:  7       6       5       4       3       2       1       0
   Pin:  9       8       7       6       5       4       3       2
   ---------------------------------------------------------------------

   Base + 1   Status I/O Address  TTL Input Only
   ---------------------------------------------------------------------
   Bit:  7 Inv   6       5       4       3       2       1       0
   Pin:  11      10      12      13      15      NC      NC      NC 
   ---------------------------------------------------------------------

   Base + 2   Control I/O Address Open-Collector Output/(Input:WR=>04h)
   ---------------------------------------------------------------------
   Bit:  7       6       5       4       3 Inv   2       1 Inv   0 Inv
   Pin:  NC      NC  NC/(BiDir) Int      17      16      14      1 
   ---------------------------------------------------------------------

   Copyright to Public Domain by Richard Steven Walz (C) 1996
   ftp://ftp.armory.com/pub/user/rstevew/LPT/simplpt.faq

   This defines the pin # to ioport mappings. offset is relative to
   lp_table[ pconfig.port ] and mask is the pins bit position in the byte
 */
struct {
  unsigned int	offset;
  unsigned char	mask;
} pin_table[26] = {
  { 1, 0x00 },			/* pin 0	Invalid pin #	*/
  { 2, 0x01 },			/* pin 1	(out)	!strobe	*/
  { 0, 0x01 },			/* pin 2	(out)	Data 0	*/
  { 0, 0x02 },			/* pin 3	(out)	Data 1	*/
  { 0, 0x04 },			/* pin 4	(out)	Data 2	*/
  { 0, 0x08 },			/* pin 5	(out)	Data 3	*/
  { 0, 0x10 },			/* pin 6	(out)	Data 4	*/
  { 0, 0x20 },			/* pin 7	(out)	Data 5	*/
  { 0, 0x40 },			/* pin 8	(out)	Data 6	*/
  { 0, 0x80 },			/* pin 9	(out)	Data 7	*/
  { 1, 0x40 },			/* pin 10	(in)	!ack	*/
  { 1, 0x80 },			/* pin 11	(in)	busy	*/
  { 1, 0x20 },			/* pin 12	(in)	Pout	*/
  { 1, 0x10 },			/* pin 13	(in)	Select	*/
  { 2, 0x02 },			/* pin 14	(out)	!feed	*/
  { 1, 0x08 },			/* pin 15	(in)	!error	*/
  { 2, 0x04 },			/* pin 16	(out)	!init	*/
  { 2, 0x08 },			/* pin 17	(out)	!SI	*/
  { 1, 0x00 },			/* pin 18	GND		*/
  { 1, 0x00 },			/* pin 19	GND		*/
  { 1, 0x00 },			/* pin 20	GND		*/
  { 1, 0x00 },			/* pin 21	GND		*/
  { 1, 0x00 },			/* pin 22	GND		*/
  { 1, 0x00 },			/* pin 23	GND		*/
  { 1, 0x00 },			/* pin 24	GND		*/
  { 1, 0x00 },			/* pin 25	GND		*/
};


/* ------------------------------------------------------------------------
   Delay for a number of uS -- Works better than usleep for me!
   ------------------------------------------------------------------------ */
void waitus( unsigned int us )
{
  struct timeval tv, wt;
    
  gettimeofday( &tv, NULL );
  
  wt.tv_usec = ( tv.tv_usec + us ) % 1000000;
  wt.tv_sec = tv.tv_sec + ( ( tv.tv_usec + us ) / 1000000);
  
  /* loop until time is up */
  for (;;)
  {
    gettimeofday( &tv, NULL );
    if ( tv.tv_sec > wt.tv_sec )
      return;
    if ( tv.tv_sec == wt.tv_sec )
      if ( tv.tv_usec > wt.tv_usec )
        return;
  }   
}



/* -----------------------------------------------------------------------
   Port control routines -- easier than asm/io.h's routines
   ----------------------------------------------------------------------- */
static inline void
pp_outb ( char value, short port)
{
  waitus( 1 );

  __asm__ __volatile__ ("outb %b0,%w1": :"a" (value), "Nd" (port));
}

static inline unsigned char
pp_inb (short port)
{
  unsigned int ret;
  
  waitus( 1 );

  __asm__ __volatile__ ("inb %w1,%0":"=a" (ret):"Nd" (port));
  
  return ret;
}



/* ----------------------------------------------------------------------- 
   Low Level programmer control

   pconfig has the pins and polarity of the various control lines
   pin_table[] has the offset and mask for eack of the DB-25 pins.
   ----------------------------------------------------------------------- */

/* turn on an output */
int biton( int pin, int pol )
{
  if( pol == 1 )
  {
    lp_data[pin_table[pin].offset] |= pin_table[pin].mask;
  } else {
    lp_data[pin_table[pin].offset] &= ~pin_table[pin].mask;
  }
  pp_outb( lp_data[pin_table[pin].offset], lp_table[pconfig.port] + pin_table[pin].offset );

  return TRUE;
}

/* turn off an output */
int bitoff( int pin, int pol )
{
  if( pol == 1 )
  {
    lp_data[pin_table[pin].offset] &= ~pin_table[pin].mask;
  } else {
    lp_data[pin_table[pin].offset] |= pin_table[pin].mask;
  }
  pp_outb( lp_data[pin_table[pin].offset], lp_table[pconfig.port] + pin_table[pin].offset );

  return TRUE;
}



/* Input a bit */
int inbit()
{
  /* If the data is not inverted, return the normal state. Otherwise,
     invert it
   */
  if( pconfig.DATAI_pol == 1 )
  {
    return( pp_inb( lp_table[pconfig.port] + pin_table[pconfig.DATAI_pin].offset )
                 & pin_table[pconfig.DATAI_pin].mask );
  } else {
    return( !pp_inb( lp_table[pconfig.port] + pin_table[pconfig.DATAI_pin].offset )
                 & pin_table[pconfig.DATAI_pin].mask );
  }
}

/* Vpp ON */
int vppon()
{
 return( biton( pconfig.VPP_pin, pconfig.VPP_pol ) );
}

/* Vpp OFF */
int vppoff()
{
  return( bitoff( pconfig.VPP_pin, pconfig.VPP_pol ) );
}

/* Vdd ON */
int vddon()
{
  return( biton( pconfig.VDD_pin, pconfig.VDD_pol ) );
}

/* Vdd OFF */
int vddoff()
{
  return( bitoff( pconfig.VDD_pin, pconfig.VDD_pol ) );
}

/* Clock high */
int clkhi()
{
  return( biton( pconfig.CLK_pin, pconfig.CLK_pol ) );
}

/* Clock low */
int clklo()
{
  return( bitoff( pconfig.CLK_pin, pconfig.CLK_pol ) );
}

/* Data high */
int outhi()
{
  return( biton( pconfig.DATAO_pin, pconfig.DATAO_pol ) );
}

/* Data low */
int outlo()
{
  return( bitoff( pconfig.DATAO_pin, pconfig.DATAO_pol ) );
}


/* ----------------------------------------------------------------------- 
   Send a 6 bit command to the PIC
   ----------------------------------------------------------------------- */
void send_cmnd( unsigned char d )
{
  int x;

  for( x = 0; x < 6; x++ )
  {
    clkhi();				/* Clock high */
    if ( d & 0x01 )
      outhi();				/* Output a 1 */
    else
      outlo();				/* Output a 0 */
    clklo();				/* Clock lo */
    d = d >> 1;                         /* Move the data over 1 bit */
  }
  outhi();				/* Added for ITU-1 support */
}


/* ----------------------------------------------------------------------- 
   Send 16 bits of data to the PIC
   clock idles low, change data. Bring clock high for 100nS, then low again
   for a minimum of 100nS
   ----------------------------------------------------------------------- */
void send_data( unsigned int d )
{
  int x;

  d *= 2;                             /* Multiply by 2 to insert start bit */
  d &= 0x7FFE;                        /* Make sure stop & start bits are zero */
  for( x = 0; x < 16; x++ )
  {
    clkhi();                  		/* Clock high */
    if ( d & 0x01 )
      outhi();                		/* Data bit is a one */
    else
      outlo();                		/* Data bit is a zero */
    clklo();
    d = d >> 1;                       	/* Move the data over 1 bit */
  }
  outhi();				/* Added for ITU-1 support */
}


/* ----------------------------------------------------------------------- 
   Read 14 bits of data from the PIC
   clock idles low, change data. 1 start bit, 1 stop bit, data valid on
   falling edge.
   ----------------------------------------------------------------------- */
unsigned int get_data( void )
{
  unsigned int x, ind;

  ind = 0;

  outhi();					/* Added for ITU-1 support */
  for( x = 0; x < 16; x++ )
  {
    clkhi();
    clklo();
    if ( inbit() )
      ind |= 0x8000;				/* Bit is zero	*/
    else
      ind &= 0x7FFF;				/* Bit is one	*/
    ind = ind >> 1;                   		/* Shift data over by 1 */
  }
  return ( ind & 0x3FFF );
}


/* -----------------------------------------------------------------------
   Find the port information from /proc/ioports

   Lee just used the current table (3 entries).
   It ought to be expanded to dynamically allocate space for any number of
   parallal ports.
   ----------------------------------------------------------------------- */
void init_lp_table( void ) {
  char buf[80];
  FILE *fp;
  int  start;
  int  stop;
  int  id;

  for( id=0; id<NUM_PORTS; id++ ) {
    lp_table[id] = 0;
  }
  max_lp = 0;

  /* Read /proc/ioports and get the base address for parport entries */
  if( (fp = fopen( "/proc/ioports", "r" )) != NULL ) {
    while( fgets( buf, 80, fp ) != NULL ) {
      /* search for parallel ports and add them to the list */
      if( sscanf( buf, "%x-%x : parport%d", &start, &stop, &id ) == 3 ) {
        if( id < NUM_PORTS ) {
	  lp_table[id] = start;
	  if(id > max_lp) {
	    max_lp = id;
	  }
	} /* NUM_PORTS */
      } /* sscanf */
    } /* readin /proc/ioports */
  } /* opening /proc/ioports */
  fclose( fp );
}



/* -----------------------------------------------------------------------
   Try to initalize the LP port, find it's base address etc. to make sure
   that it is useable.
   ----------------------------------------------------------------------- */
int init_port()
{
  char	printer[10];
  int	fd;

  /* Make sure the port number is within the proper range */
  if( (pconfig.port > NUM_PORTS) || (pconfig.port > max_lp) ) {
    fprintf(stderr, "Error, lp%d is not supported, %d is > maximum port %d\n",
	    pconfig.port, pconfig.port, max_lp );
    return 0;
  }

  if( lp_table[pconfig.port] == 0 ) {
    fprintf(stderr, "Error, base address for %d is 0x0000\n", pconfig.port );
    return 0;
  }

  sprintf(printer,"/dev/lp%d", pconfig.port );

  /* check for access to the printer */
  if( access( printer, R_OK|W_OK ) < 0 ) {
    fprintf( stderr, "You don't have permission to access %s\n", printer );
    return 0;
  }

  fd=open(printer,O_WRONLY);
  if ( fd<0 )
  {
    perror(NULL);
    return 0;
  }
  
  if ( options & OPT_DEBUG )  
    printf( "I/O base address for %s is 0x%x\n", printer, lp_table[pconfig.port] );

  if( lp_table[pconfig.port] < 0x3ff ) {
    /* Get permission to access the config register */
    if ( ioperm( lp_table[pconfig.port], 3L, 1) <0 )
      {
	fprintf( stderr, "Access to port 0x%x denied\n", lp_table[pconfig.port] );
	return 0;
      }
  } else {
    /* High port address, try to get access. This is a really, really bad
       way to do this, as it exposes ALL the ports I believe. 
    */
    if( iopl(3) < 0 ) {
        perror(NULL);
	fprintf( stderr, "Access to port 0x%x denied\n", lp_table[pconfig.port]+1 );
	return 0;
    }
  }

  /* Get the initial states of data and command line registers */  
  lp_data[0] = pp_inb( lp_table[pconfig.port] + 0 );
  lp_data[1] = pp_inb( lp_table[pconfig.port] + 1 );
  lp_data[2] = pp_inb( lp_table[pconfig.port] + 2 );
  
  if ( options & OPT_DEBUG )
    printf( "data = %02X   status = %02X   cmnd = %02X\n",
	    lp_data[0], lp_data[1], lp_data[2] );

  return 1;
}


/* -----------------------------------------------------------------------
   Verify the PIC's memory against RAM buffer. Show errors

   returns TRUE if it verified ok
   returns FALSE if it failed
   ----------------------------------------------------------------------- */
int verify_pic( WINDOW *scrollw )
{
  int x, status;
  unsigned int picd;

  status = TRUE;
    
  /* Make clock and data lines low before !MCLR rises */
  clklo();
  outlo();
      
  /* Set mode to program */
  vddon();

  waitus( 1 );
    
  /* Turn on Vpp voltage */
  vppon();

  waitus( 1 );

  /* Verify main part of pic memory */  
  for ( x = 0; x < 0x400; x++ )
  {
    send_cmnd( 0x04 );				/* Read data */
    waitus( 1 );

    /* See if data is the same as memory. Print bad addresses */
    picd = get_data();
    
    if ( picd != ( pic.mem[x] & 0x3FFF ) )
    {
      if( scrollw != NULL )
      {
        wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
        wrefresh( scrollw );
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
	    flushall();
	  }
      }
      status = FALSE;			/* Did not Verify */
    } else {
      /* Only show every 16 words */
      if( ( x % 16) == 0 )
      {
        if( scrollw != NULL )
        {
          mvprintw( 3, 34, "0x%04X", x );
          refresh();
        } else {
	  if( !(options & OPT_QUIET) )
	    {
	      printf("0x%04X\r", x );
	      flushall();
	    }
        }
      }
    }

    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }

  /* Verify Pic's EEPROM data memory */
  for ( x = pconfig.EEaddr; x < pconfig.EEaddr+64; x++ )
  {
    send_cmnd( 0x05 );			/* Read Data Memory */
    waitus( 1 );
    picd = get_data() & 0x00FF;

    if ( picd != ( pic.mem[x] & 0x00FF ) )
    {
      if( scrollw != NULL )
      {
        wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x00FF );
        wrefresh( scrollw );
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x00FF );
	    flushall();
	  }
      }
      status = FALSE;			/* Did not Verify */
    } else {
      /* Only show every 16 words */
      if( ( x % 16) == 0 )
      {
        if( scrollw != NULL )
        {
          mvprintw( 3, 34, "0x%04X", x );
          refresh();
        } else {
	  if( !(options & OPT_QUIET) )
	    {
	      printf( "0x%04X\r", x );
	      flushall();
	    }
        }
      }
    }
    
    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }

  /* Verify Pic's ID */
  send_cmnd( 0x00 );			/* Set PC to 0x2000 */  
  send_data( 0x3FFF );			/* Dummy data */
  
  waitus( 1 );
  for( x = pconfig.IDaddr; x < pconfig.IDaddr+4; x++ )
  {
    send_cmnd( 0x04 );			/* Read Program Memory */
    waitus( 1 );
    picd = get_data();

    if ( picd != ( pic.mem[x] & 0x3FFF ) )
    {
      if( scrollw != NULL )
      {
        wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
        wrefresh( scrollw );
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
	    flushall();
	  }
      }
      status = FALSE;			/* Did not Verify */
    } else {
      if( scrollw != NULL )
      {
        mvprintw( 3, 34, "0x%04X", x );
        refresh();
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    printf("0x%04X\r", x );
	    flushall();
	  }
      }
    }

    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }
  
  for ( x = 0; x < 3; x++ )
  {
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }
  
  send_cmnd( 0x04 );			/* Read program Memory */
  waitus( 1 );
  picd = get_data();			/* Read Fuse data */

  if ( picd != ( pic.mem[pconfig.CFaddr] & 0x3FFF ) )
  {
    if( scrollw != NULL )
    {
      wprintw( scrollw, "%04X: %04X != %04X  ", pconfig.CFaddr, picd,
   					 pic.mem[pconfig.CFaddr] & 0x3FFF );
      wrefresh( scrollw );
    } else {
      if( !(options & OPT_QUIET) )
	{
	  fprintf( stderr, "%04X: %04X != %04X  ", pconfig.CFaddr, picd,
		                                   pic.mem[pconfig.CFaddr] & 0x3FFF );
	  flushall();
	}
    }
    status = FALSE;			/* Did not Verify */
  } else {
    if( scrollw != NULL )
    {
      mvprintw( 3, 34, "0x%04X", pconfig.CFaddr );
      refresh();
    } else {
      if( !(options & OPT_QUIET) )
	{
	  printf( "0x%04X\r", pconfig.CFaddr ); 
	  flushall();
	}
    }
  }

  waitus( 1 );
    
  /* Turn off Vpp voltage */
  vppoff();

  waitus( 1 );
  
  /* Turn off program mode */
  vddoff();

  return status;
}


/* -----------------------------------------------------------------------
   Program our RAM buffer into the PIC
   ----------------------------------------------------------------------- */
int program_pic( WINDOW *scrollw )
{
  int x, y;
  unsigned int picd;
  
  /* Make clock and data lines low before !MCLR rises */
  clklo();
  outlo();
      
  /* Set mode to program */
  vddon();

  waitus( 1 );
    
  /* Turn on Vpp voltage */
  vppon();

  waitus( 1 );

  /* Erase chip/Disable code protect from MicroChip 2.2.2 	*/
  send_cmnd( 0x00 );			/* Load Configuration 	*/
  waitus( 1 );
  send_data( 0x3FFF );			/* Clear Configuration	*/
  waitus( 1 );
  
  for ( x = 0; x < 7; x++ )
  {
    send_cmnd( 0x06 );			/* Increment PC		*/
    waitus( 1 );
  }
    
  send_cmnd( 0x01 );			/* ??			*/
  waitus( 1 );
  send_cmnd( 0x07 );			/* ??			*/
  waitus( 1 );
  send_cmnd( 0x08 );			/* Begin programming	*/
  waitus( 10000 );			/* Wait 10mS		*/
  send_cmnd( 0x01 );
  waitus( 1 );
  send_cmnd( 0x07 );
  waitus( 1 );

  /* Turn off Vpp voltage */
  vppoff();
  waitus( 1 );
  /* Turn off program mode */
  vddoff();

  waitus( 65000 );
  
  /* Make clock and data lines low before !MCLR rises */
  clklo();
  outlo();
     
  /* Set mode to program */
  vddon();
  waitus( 1 );
  /* Turn on Vpp voltage */
  vppon();

  waitus( 1 );

  /* Program the PIC's main program code memory */  
  for ( x = 0; x < 0x400; x++ )
  {
    send_cmnd( 0x02 );			/* Load data */
    waitus( 1 );
    send_data( pic.mem[x] );
    waitus( 1 );
    send_cmnd( 0x08 );			/* Begin programming */
    waitus( 10000 );			/* 10mS programming delay */

    /* Read the data back and report any errors */
    send_cmnd( 0x04 );
    picd = get_data();

    if ( picd != ( pic.mem[x] & 0x3FFF ) )
    {
      if ( scrollw != NULL )
      {
        wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF );
        wrefresh( scrollw );
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF );
	    flushall();
	  }
      }
    } else {
      /* Only print every 16 addresses */
      if( (x % 16) == 0 )
      {
        if( scrollw != NULL )
        {
          mvprintw( 3, 34, "0x%04X", x );	/* Show the address */
          refresh();
        } else {
	  if( !(options & OPT_QUIET) )
	    {
	      printf( "0x%04X\r", x );		/* Show the address on stdout */
	      flushall();
	    }
        }
      }
    }
    
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }

  /* Program the EEPROM data area */
  for( x = pconfig.EEaddr; x < pconfig.EEaddr+64; x++ )
  {
    send_cmnd( 0x03 );			/* Load data command */
    waitus( 1 );
    send_data( pic.mem[x] );
    waitus( 1 );
    send_cmnd( 0x08 );			/* Begin programming */
    waitus( 10000 );			/* 10mS programming delay */

    send_cmnd( 0x05 );
    picd = get_data() & 0x00FF;
    if ( picd != ( pic.mem[x] & 0x00FF ) )
    {
      if ( scrollw != NULL )
      {
        wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x00FF );
        wrefresh( scrollw );
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x00FF );
	    flushall();
	  }
      }
    } else {
      /* Only print every 16 addresses */
      if( (x % 16) == 0 )
      {
        if( scrollw != NULL )
        {
          mvprintw( 3, 34, "0x%04X", x );	/* Show the address */
          refresh();
        } else {
	  if( !(options & OPT_QUIET) )
	    {
	      printf( "0x%04X\r", x );		/* Show the address */
	      flushall();
	    }
        }
      }
    }

    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }

  /* Program the ID and fuse configurations */
  send_cmnd( 0x00 );		/* Load configuration. Sets PC to 0x2000 */
  waitus( 1 );
  send_data( 0x3FFF );		/* Dummy data load */
  waitus( 1 );
    
  for( x = pconfig.IDaddr; x < pconfig.IDaddr+4; x++ )
  {
    /* Try programming the location 25 times before giving up */
    y = 0;
    do
    {
      if ( y > 0 )
      {
        if ( scrollw != NULL )
        {
          wprintw( scrollw, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
          wrefresh( scrollw );
        } else {
	  if( !(options & OPT_QUIET) )
	    {
	      fprintf( stderr, "%04X: %04X != %04X  ", x, picd, pic.mem[x] & 0x3FFF);
	      flushall();
	    }
        }
      }

      
      send_cmnd( 0x02 );		/* Load data command */
      waitus( 1 );
      send_data( pic.mem[x] );	/* Program ID data */
      waitus( 1 );
      send_cmnd( 0x08 );		/* Begin programming */
      waitus( 10000 );

      send_cmnd( 0x04 );		/* Read what was just written */
      waitus( 1 );
      picd = get_data();
      waitus( 1 );
      
      if( scrollw != NULL )
      {
        mvprintw( 3, 34, "0x%04X", x );		/* Show the address */
        refresh();
      } else {
	if( !(options & OPT_QUIET) )
	  {
	    printf( "0x%04X\r", x );		/* Show the address */
	    flushall();
	  }
      }
    } while ( (y++ < 25) && ( picd != (pic.mem[x] & 0x3FFF) ) );
    
    /* Program location 3x number of failures */
    y *= 3;
    for( ; y > -1; y-- )
    {
      send_cmnd( 0x02 );		/* Load data command */
      waitus( 1 );
      send_data( pic.mem[x] );
      waitus( 1 );
      send_cmnd( 0x08 );		/* Begin programming */
      waitus( 10000 );
    }
    
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }
  
  send_cmnd( 0x06 );			/* point to 2005 */
  waitus( 1 );
  send_cmnd( 0x06 );			/* point to 2006 */
  waitus( 1 );
  send_cmnd( 0x06 );			/* point to 2007 -- fuses */
  waitus( 1 );
    
  send_cmnd( 0x02 );			/* Load data command */
  waitus( 1 );
  send_data( pic.mem[pconfig.CFaddr] );	/* Write fuse data */
  waitus( 1 );
  send_cmnd( 0x08 );			/* Begin program cycle */  
  waitus( 10000 );			/* 10mS program delay */

  send_cmnd( 0x04 );			/* Read data */
  picd = get_data();
  if ( picd != ( pic.mem[pconfig.CFaddr] & 0x3FFF ) )
  {
    if ( scrollw != NULL )
    {
      wprintw( scrollw, "%04X: %04X != %04X  ", pconfig.CFaddr, picd, pic.mem[pconfig.CFaddr] & 0x3FFF);
      wrefresh( scrollw );
    } else {
      if( !(options & OPT_QUIET) )
	{
	  fprintf( stderr, "%04X: %04X != %04X  ", pconfig.CFaddr, picd, pic.mem[pconfig.CFaddr] & 0x3FFF);
	  flushall();
	}
    }
  } else {
    if( scrollw != NULL )
    {
      mvprintw( 3, 34, "0x%04X", pconfig.CFaddr );	/* Show the address */
      refresh();
    } else {
      if( !(options & OPT_QUIET) )
	{
	  printf( "0x%04X\r", pconfig.CFaddr );	/* Show the address */
	  flushall();
	}
    }
  }
 
  waitus( 1 );
    
  /* Turn off Vpp voltage */
  vppoff();

  waitus( 1 );
  
  /* Turn off program mode */
  vddoff();
  waitus( 1 );

  return TRUE;
}


/* -----------------------------------------------------------------------
   Read the PIC's memory into our RAM buffer
   ----------------------------------------------------------------------- */
int read_pic( WINDOW *scrollw )
{
  int x;

  /* Make clock and data lines low before !MCLR rises */
  clklo();
  outlo();
    
  /* Set mode to program */
  vddon();

  waitus( 1 );
    
  /* Turn on Vpp voltage */
  vppon();

  waitus( 1 );

  /* Read the PIC's Memory map */  
  for ( x = 0; x < 0x400; x++ )
  {
    send_cmnd( 0x04 );			/* Read Program Memory */
    waitus( 1 );
    pic.mem[x] = get_data();
    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
    
    /* Show every 16th memory address */
    if( (x % 16) == 0 )
    {
      if( scrollw != NULL )
      {
        mvprintw( 3, 34, "0x%04X", x );
        refresh();
      } else {
         printf( "0x%04X\r", x );
         flushall();
      }
    }
  }

  /* Read Pic's data area into memory */
  for ( x = pconfig.EEaddr; x < pconfig.EEaddr+64; x++ )
  {
    send_cmnd( 0x05 );			/* Read Data Memory */
    waitus( 1 );
    pic.mem[x] = get_data() & 0xFF;
    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );

    /* Show every 16th memory address */
    if( (x % 16) == 0 )
    {
      if( scrollw != NULL )
      {
        mvprintw( 3, 34, "0x%04X", x );
        refresh();
      } else {
        printf( "0x%04X\r", x );
        flushall();
      }
    }
  }

  /* Read ID and fuse info into structure */
  send_cmnd( 0x00 );			/* Set PC to 0x2000 */  
  send_data( 0x3FFF );			/* Dummy data */
  	
  waitus( 1 );
  for( x = pconfig.IDaddr; x < pconfig.IDaddr+4; x++ )
  {
    send_cmnd( 0x04 );			/* Read Program Memory */
    waitus( 1 );
    pic.mem[x] = get_data();
    waitus( 1 );
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );

    if( scrollw != NULL )
    {
      mvprintw( 3, 34, "0x%04X", x );
      refresh();
    } else {
       printf( "0x%04X\r", x );
       flushall();
    }
  }
  
  for ( x = 0; x < 3; x++ )
  {
    send_cmnd( 0x06 );			/* Increment address */
    waitus( 1 );
  }
  
  send_cmnd( 0x04 );			/* Read program Memory */
  waitus( 1 );
  pic.mem[pconfig.CFaddr] = get_data();	/* Read Fuse data */
  waitus( 1 );

  if( scrollw != NULL )
  {
    mvprintw( 3, 34, "0x%04X", pconfig.CFaddr );
    refresh();
  } else {
    printf( "0x%04X\r", pconfig.CFaddr );
    flushall();
  }
    
  /* Turn off Vpp voltage */
  vppoff();

  waitus( 1 );
  
  /* Turn off program mode */
  vddoff();
       
  return TRUE;
}
   
