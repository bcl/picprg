/* --------------------------------------------------------------------- *
   Linux PIC16C84 programming software

   Supports Russ Reiss's PICSPA84 PIC16C84 programmer board
   and David Tait's PD PIC16C84 programmer schematic and probably any
   custom parallel port programmer.
   
   Uses parallel port to control programmer
   
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
   
   =====================================================================
   08/23/2002   Read a different pic, it had a program already in it.
   bcl          Wrote it to the 3rd PIC, which was all blank before prog.
                Looks like I needed to turn it all off and try again, the
		other pic also programs ok now.
		Is it really needed? I set it to 0 and it still works fine
		on my 1GHz laptop. Removing pconfig.delay from the program.
		What's left? I need to go through my email archives and
		find any useful patches that people have sent. I remember
		something to do with the configuration bits a while back.
		I also need to look at what variations there are for the
		part and if there are simple changes add them.
		Any support for more complicated changes will come in the
		next version.

   08/21/2002   I changed read_pic() to use pconfig.delay in all of its
   bcl          waitus() calls, with no improvement in performence. When
                looking at the data line while reading the part it looks
		like it may really be reading zeros, the data pin pattern
		looks regular when viewed on the 10mS scale.

		In read_pic the Vpp and Vdd go high as soon as I set
		clk low, which means that it may not go into read mode
		correctly. I need to find the schematics for this pgmr
		and see if this is normal or if there is something in the
		the program doing that.

		The ~ that were changed in lowlvl.c should not have been
		changed.
		It almost works. It looks like it erases ok, but programming
		doesn't work yet. It writes several words near the start, but
		not the rest of the PIC.
		Read appears to work most of the time. I saw 1 case where
		re-reading it resulted in different values in memory.

   08/19/2002   The pins are not being correctly configured when the program
   bcl          runs or when it is exited.
                clklo() call is causing the problem. It turns on the Vdd
		and Vpp power. I changed the order they are called in and
		power now stays off. I need to check to see if this is a
		function of the programmer or something wrong in the pin
		table.

		It also doesn't read the PIC. It is reading all zeros. 
		Speed may be a factor, but I have increased the new delay
		value and it still reads zeros.
		


   08/18/2002   Going through code looking for potential buffer overflows.
   bcl          Since this code is usually suid root (for port access) I
                need to be very careful about strings, etc. What it really
		needs is to be rewritten to use the parallel port low level
		driver module.
		The xterm size isn't that same as when I wrote this, the
		25 line (0-24) may not be visible. Added lastline=23 
		variable. Nothing changes it at this time.

		It looks like setting pins on/off in the config menu
		are interfering with other pins. Although this could be
		the programmer?

		It also isn't properly turning off the control lines when
		exiting the config state or the program.

   08/17/2002   Adding /proc/ioports code from Lee Olsen. Changing the way
   bcl          base addresses are stored and used in lowlvl.c
                Adding GPL to all the project files.

   07/05/98     Changing version # to v2.3.0
                Adding a useful command line interface, mostly already
		in place.
		Adding command line verify option
		Adding command line quiet option (no output of
		errors/status, just return code)

   05/29/98	Changing Version # to v2.2
   		Finally added the extra outhi() calls to lowlvl.c so that
   		the ITU-1 programmer will work. Testing with my programmer
   		to make sure it works.

   01/01/98	Finishing up the fixes/changes
   		Added display of addresses during read, verify, write
   		
   12/31/97	Changing Version # to v2.1
   		Fixing bugs and adding features
   		x 1. Ian's intel16 code really isn't. Removing and writing
   		     my own intel hex load routines.
   		x 2. Adding a monochrome mode (-m)
   		x 3. Fixing an error with the fuse being programmed wrong?
   		     or maybe recalled wrong from the data file?
   		4. Add a goto memory address command to memory display
   		5. Add home and end keys to memory display

   11/20/97	Dropping support for pictools since it won't work and
   		I cannot find Ian King anywhere. Changing rcfile to be
   		~/.picprgrc

   11/18/97	Well, pictools v0.8 do not work on my machine. I cannot
   		track down Ian, so I'll leave my pictools loading the
   		same as it has been, although it appears that he reverted
   		to the 'old' pictools .cfg format for pictools v0.8,
   		rather than the newer format he was using in pictools v0.7
   		which I don't have anymore either.

   11/15/97	Revising this to work with kernel 2.0.31 for me. Its
   		been a long time since I've even run this program. I
   		have modified my Russ Reiss programmer to act like a
   		David Tait programmer with inverted lines (I forget
   		which of course). I need to add a line test routine
   		so that users can eaily get their programmer configured
   		correctly.
   		Added a -d debugging flag.
		Well, its gets to the color setup for ncurses now. But
		since I'm running in a xterm it fails... ncurses or
		a termcap problem?
		ioperm needed a 1L paramter passed, not a 1. Newer
		GCC must be more strict, or doesn't convert?
		OK, setting TERM=xterm-color lets it run in a xterm
		TODO
		  User defined programmer setup. Specify the DB-25 pin
		  number and the logic state (+ or -)
		  Test program that will toggle the selected lines
		  on and off (like Vpp on, etc.)
		  Support other PIC processors (what's the diff?)
		  Better feedback while reading and writing (user
		  selectable if it slows things down considerably).

 * --------------------------------------------------------------------- */
#include <stdio.h>
#include <ctype.h>
#include <getopt.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "picprg.h"

extern int	optind;			/* Option index			*/
extern int      max_lp;                 /* from lowlvl.c                */

int	        options=0;              /* Command line flags
					   0x0001 = quiet mode
					   0x0002 = verify mode
					   0x0004 = debug mode
					*/          
int             lastline=23;            /* Last line of display         */
struct _pic	pic;			/* PIC memory map		*/
struct _pconfig	pconfig; 		/* port configuration		*/


/* -------------------------------------------------------------------------
   Set window to attribute 'attr' and clear it with spaces
   ------------------------------------------------------------------------- */
void attr_clear(WINDOW *win, int width, int height, chtype attr)
{
  int i, j;

  wattrset(win, attr);   	 	/* Set window to attribute 'attr' */
  for (i = 0; i < height; i++) {
    wmove(win, i, 0);
    for (j = 0; j < width; j++)
      waddch(win, ' ');
  }
  wmove( win, 0, 0 );			/* Put cursor at top right */
}


/* -----------------------------------------------------------------------
   Title line
   ----------------------------------------------------------------------- */
void title()
{
  char xtal[3], temp[80], temp1[40];

  attr_clear( stdscr, 80, 25, COLOR_PAIR( 1 ) );

  attron( A_REVERSE );
  sprintf( temp, " Linux PIC84 programmer     v%s         Copyright 1994-2002 by Brian C. Lane ", VERSION );
  addstr( temp );
  attrset( COLOR_PAIR( 1 ) );
    
    switch ( pic.mem[pconfig.CFaddr] & 0x0003 )
    {
      case 0x00:	strcpy( xtal, "LP" );	/* LP clock */
    			break;
    		
      case 0x01:	strcpy( xtal, "XT" );	/* XT clock */
	    		break;
    		
      case 0x02:	strcpy( xtal, "HS" );	/* HS clock */
    			break;
      
      case 0x03:	strcpy( xtal, "RC" );	/* RC clock */
      			break;
    		
      default:		strcpy( xtal, "xx" );	/* Not defined */
                	break;
    }

    /* White on Red background. 2nd line */
    attrset( COLOR_PAIR( 2 ) );
    sprintf( temp, "  PIC16C84 CLOCK: %s  ",
                xtal );
    sprintf( temp1, "CP: %s  WD: %s  PU: %s  ",
    	  	(pic.mem[pconfig.CFaddr] & 0x10) ? "OFF" : "ON ",
    	  	(pic.mem[pconfig.CFaddr] & 0x04) ? "ON " : "OFF",
    	  	(pic.mem[pconfig.CFaddr] & 0x08) ? "ON " : "OFF" );
    mvprintw( 1, 0, "%-40s%40s", temp, temp1 );
    attrset( COLOR_PAIR( 1 ) );

  refresh();
}


/* -----------------------------------------------------------------------
   Ask user to press any key to continue
   ----------------------------------------------------------------------- */
void press_any()
{
  attron( A_REVERSE );
  mvaddstr( lastline, 0, "                           Press any key to continue                            " );
  attrset( COLOR_PAIR( 1 ) );
  move( lastline, 0 );
  refresh();
  
  getch();
}


/* -----------------------------------------------------------------------
   Erase the error line and the 'press any key' line.
   ----------------------------------------------------------------------- */
void erase_err()
{
  move( 22, 0 );
  clrtoeol( );
  move( lastline, 0 );
  clrtoeol( );
}


/* -----------------------------------------------------------------------
   Show an error string on line 22, 0 in Red on White
   ----------------------------------------------------------------------- */
void show_err( char *fmt, ... )
{
  va_list	ap;
  int		l;
  char		temp[81];
  
  va_start( ap, fmt );
  vsprintf( temp, fmt, ap );
  
  l = strlen( temp );
  attrset( COLOR_PAIR( 6 ) );
  mvprintw( 22, 0, "%*s%s%*s", (80-l) / 2, " ", temp, (80-l) / 2, " " );

  /* If roundoff error, add a space to end of string */
  if ( ( ( ( ( 80-l ) / 2 ) * 2 ) + l ) != 80 )
    addstr( " " );
  attrset( COLOR_PAIR( 1 ) );  
}


/* -----------------------------------------------------------------------
   Read in configuration data from ~/.picprgrc data file
   ----------------------------------------------------------------------- */
void read_config()
{
  FILE	*fp;
  char	path[MAXPATHLEN];
    
  /* Defaults: */
  pconfig.port = 0;				/* /dev/lp port		*/
  pconfig.VPP_pin = 5;				/* VPP on pin 5		*/
  pconfig.VPP_pol = 1;				/* Normal by default	*/
  pconfig.VDD_pin = 4;				/* Vdd on pin 4		*/
  pconfig.VDD_pol = 1;				/* Normal by default	*/
  pconfig.CLK_pin = 3;				/* Clock on pin 3	*/
  pconfig.CLK_pol = 1;				/* Normal by default	*/
  pconfig.DATAO_pin = 2;			/* Data to PIC on pin 2	*/
  pconfig.DATAO_pol = 1;			/* Normal by default	*/
  pconfig.DATAI_pin = 10;			/* Data from pic on p10 */
  pconfig.DATAI_pol = 0;			/* Inverted by default	*/
  pconfig.IDaddr = 0x2000;			/* Default ID locaiton	*/
  pconfig.EEaddr = 0x2100;			/* Default EE location	*/
  pconfig.CFaddr = 0x2007;			/* Config Fuses		*/
  pconfig.colors = TRUE;			/* Color by default	*/

  /* Make a path to the configuration file */
  strncpy( path, getenv( "HOME" ), MAXPATHLEN );
  strncat( path, "/.picprgrc", MAXPATHLEN );

  if ( ( fp = fopen( path, "r" ) ) == NULL )
  {
    return;
  }
  
  /* Read the file 1 line at a time */
  while ( fgets( path, 80, fp ) != NULL )
  {
    /* Ignore comments and blank lines */
    if ( path[0] == '#' || path[0] == '\n' )
      continue;

    /* Set the /dev/lp port to use */
    if ( strncmp( path, "port=", 5 ) == 0 )
    {
      pconfig.port = atoi( &path[5] );
      continue;
    }

    /* Set the VPP pin number and polarity */
    if( strncmp( path, "vpp=", 4 ) == 0 )
    {
      if( (atoi( &path[5] ) < 26) && (atoi( &path[5] ) > 0) )
      { 
        pconfig.VPP_pin = atoi( &path[5] );
        if( path[4] == '+' )
          pconfig.VPP_pol = 1;
        else if( path[4] == '-' )
          pconfig.VPP_pol = 0;
      }
    }

    /* Set the Vdd pin number and polarity */
    if( strncmp( path, "vdd=", 4 ) == 0 )
    {
      if( (atoi( &path[5] ) < 26) && (atoi( &path[5] ) > 0) )
      { 
        pconfig.VDD_pin = atoi( &path[5] );
        if( path[4] == '+' )
          pconfig.VDD_pol = 1;
        else if( path[4] == '-' )
          pconfig.VDD_pol = 0;
      }
    }

    /* Set the Clock pin number and polarity */
    if( strncmp( path, "clock=", 6 ) == 0 )
    {
      if( (atoi( &path[7] ) < 26) && (atoi( &path[7] ) > 0) )
      { 
        pconfig.CLK_pin = atoi( &path[7] );
        if( path[6] == '+' )
          pconfig.CLK_pol = 1;
        else if( path[6] == '-' )
          pconfig.CLK_pol = 0;
      }
    }

    /* Set the Data to PIC pin number and polarity */
    if( strncmp( path, "datao=", 6 ) == 0 )
    {
      if( (atoi( &path[7] ) < 26) && (atoi( &path[7] ) > 0) )
      { 
        pconfig.DATAO_pin = atoi( &path[7] );
        if( path[6] == '+' )
          pconfig.DATAO_pol = 1;
        else if( path[6] == '-' )
          pconfig.DATAO_pol = 0;
      }
    }
    /* Set the Data from PIC pin number and polarity */
    if( strncmp( path, "datai=", 6 ) == 0 )
    {
      if( (atoi( &path[7] ) < 26) && (atoi( &path[7] ) > 0) )
      { 
        pconfig.DATAI_pin = atoi( &path[7] );
        if( path[6] == '+' )
          pconfig.DATAI_pol = 1;
        else if( path[6] == '-' )
          pconfig.DATAI_pol = 0;
      }
    }

    /* Set the location in memory map of ID data */    
    if ( strncmp( path, "idaddr=", 7 ) == 0 )
    {
      sscanf( &path[7], "%x", &pconfig.IDaddr );
      continue;
    }
    
    /* Set the location of the EEPROM data */
    if ( strncmp( path, "eeaddr=", 7 ) == 0 )
    {
      sscanf( &path[7], "%x", &pconfig.EEaddr );
      continue;
    }

    /* Set the location of the Config FUSES data */
    if ( strncmp( path, "cfaddr=", 7 ) == 0 )
    {
      sscanf( &path[7], "%x", &pconfig.CFaddr );
      continue;
    }
    
    /* Set the colors variable to TRUE or FALSE */
    if ( strncmp( path, "colors=", 7 ) == 0 )
    {
      sscanf( &path[7], "%d", &pconfig.colors );
      continue;
    }
  }
  fclose( fp );
}


/* -----------------------------------------------------------------------
   Write configuration data to ~/.picprgrc data file
   ----------------------------------------------------------------------- */
int write_config()
{
  FILE	*fp;
  char	path[MAXPATHLEN];
  
  strncpy( path, getenv( "HOME" ), MAXPATHLEN-12 );
  strcat( path, "/.picprgrc" );
  
  if ( ( fp = fopen( path, "w" ) ) == NULL )
  {
    show_err( "Error writing ~/.picprgrc" );
    return FALSE; 
  }

  fprintf( fp, "# Linux PIC programmer configuration v2.0\n");  
  fprintf( fp, "port=%d\n",     pconfig.port );
  fprintf( fp, "vpp=%c%d\n",    pconfig.VPP_pol ? '+' : '-', pconfig.VPP_pin );
  fprintf( fp, "vdd=%c%d\n",    pconfig.VDD_pol ? '+' : '-', pconfig.VDD_pin );
  fprintf( fp, "clock=%c%d\n",  pconfig.CLK_pol ? '+' : '-', pconfig.CLK_pin );
  fprintf( fp, "datao=%c%d\n",  pconfig.DATAO_pol ? '+' : '-', pconfig.DATAO_pin );
  fprintf( fp, "datai=%c%d\n",  pconfig.DATAI_pol ? '+' : '-', pconfig.DATAI_pin );
  fprintf( fp, "idaddr=%04X\n", pconfig.IDaddr );
  fprintf( fp, "eeaddr=%04X\n", pconfig.EEaddr );
  fprintf( fp, "cfaddr=%04X\n", pconfig.CFaddr );
  fprintf( fp, "colors=%d\n",   pconfig.colors );
  
  fclose( fp );

  return TRUE;
}


/* -----------------------------------------------------------------------
   Show a page of memory information
   ----------------------------------------------------------------------- */
void show_mem( int mem_start )
{
  int x, y; 
  
  /* Print memory info. 8 per line */
  for ( x = 0; x < 20; x++ )
  {
    mvprintw( x+3, 0, "%04X:  ", mem_start+(x*8) );
    for( y = 0; y < 8; y++ )
      printw( "%04X  ", pic.mem[ mem_start+(x*8)+y] );
  }
  refresh();
}


/* -----------------------------------------------------------------------
   Display the PIC memory
   Use up/down arrows, pageup/pagedn
   ----------------------------------------------------------------------- */
void edit_pic()
{
  int	mem_start, ch;

  title();				/* Title/copyright line 	*/

  mem_start = 0;			/* Start at beginning of memory */
    
  attrset( COLOR_PAIR( 3 ) );
  mvaddstr( lastline, 0, "  Arrow keys to move                                       [Q]uit to main menu  " );
  attrset( COLOR_PAIR( 1 ) );

  while ( 1 )
  {   
    show_mem( mem_start );
    move( 0, 0 );			/* Put cursor out of the way */
            
    /* Get keypresses. Q quits as does ESC and X. Arrows move directions */
    ch = getch();

    /* Check for exit keys first */
    switch( ch )
    {
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
      case 0x1B:  return;
      		  break;
      		  
      case 0x0D:
      case KEY_DOWN:
		if ( mem_start < ( MAXPICSIZE - 0xA0 ) )
		  mem_start += 0x08;
		break;
		
      case KEY_UP:
      		if ( mem_start > 0x00 )
      		  mem_start -= 0x08;
      		break;

      case ' ':      		
      case KEY_NPAGE:
      		if ( mem_start < ( MAXPICSIZE - 0x140 ) )
      		  mem_start += 0xA0;
      		else
      		  mem_start = MAXPICSIZE - 0x9F;
      		break;
      		
      case KEY_PPAGE:
      		if ( mem_start >= 0xA0 )
      		  mem_start -= 0xA0;
      		else
      		  mem_start = 0x00;
      		break;
      		
      default:	break;
    }
  }
}


/* -----------------------------------------------------------------------
   Initalize all the memory to its blank state (0xFFFF)
   ----------------------------------------------------------------------- */
void init_pic()
{
  int i;

  for(i=0; i <= MAXPICSIZE; i++ )
    pic.mem[i] = 0xffff;
}


/* -----------------------------------------------------------------------
   Load PIC data from a Intel HEX formatted file (8M or 16)
   ----------------------------------------------------------------------- */
int load_hex( char *fname )
{
  int	min, max;
  FILE	*fp;
    
  if ( ( fp = fopen( fname, "rb" ) ) == NULL )
  {
    return FALSE;
  }
  
  min = 0;
  max = MAXPICSIZE;
  if ( readihex( fp, pic.mem, &min, &max ) == FALSE )
  {
    fclose( fp );
    return FALSE;
  }

  fclose( fp );
  return TRUE;
}


/* -------------------------------------------------------------------------
   Prompt the user for the filename
   ------------------------------------------------------------------------- */
void load_file()
{
  char	fname[255];
  
  title();
  
  /* prompt for a filename */
  attrset( COLOR_PAIR( 5 ) );
  mvprintw( 5, 0, "Load an Intel HEX file (IHX16 or IHX8M)" );
  attrset( COLOR_PAIR( 1 ) );
  
  mvprintw( 7, 0, "Enter filename to load : ");
  echo();
  getstr( fname );
  noecho();
  
  mvprintw( 9, 0, "Loading %s", fname );
  refresh();

  /* Load the file */
  if ( load_hex( fname ) == FALSE )
  {
    show_err( "Error loading %s", fname );
    press_any();
    erase_err();
  }
}


/* -----------------------------------------------------------------------
   Write PIC data to a Intel Hex (IHX16) formatted file
   ----------------------------------------------------------------------- */
int write_hex( char *fname )
{
  FILE	*fp;
    
  if ( ( fp = fopen( fname, "wb" ) ) == NULL )
  {
    show_err( "Error writing %s", fname );
    return FALSE;
  }

  /* If it is a PIC16C84 then its only 1k, if its unknown, write it all */
  if( pconfig.PIC_type == PIC16C84 )
  {  
    writeihex16( fp, pic.mem, 0, 0x0400 );

    writeihex16( fp, pic.mem, pconfig.IDaddr, pconfig.IDaddr+8 );

    writeihex16( fp, pic.mem, pconfig.EEaddr, pconfig.EEaddr+64 );
  } else {
    writeihex16( fp, pic.mem, 0, MAXPICSIZE );
  }
  writeiend16( fp );

  fclose( fp );
        
  return TRUE;
}


/* -----------------------------------------------------------------------
   Write PIC data to a file.

   Written as INHX16 file format
  ----------------------------------------------------------------------- */
void write_file()
{
  char	fname[255];
  
  title();
  
  /* prompt for a filename */
  /* Do a little directory maybe? filename completion? */
  attrset( COLOR_PAIR( 5 ) );
  mvprintw( 5, 0, "Write an INHX16 file");
  attrset( COLOR_PAIR( 1 ) );
  
  mvprintw( 7, 0, "Enter filename to write : ");
  echo();
  getstr( fname );
  noecho();
  
  mvprintw( 9, 0, "Writing %s", fname );
  refresh();

  if ( write_hex( fname ) == FALSE )		/* Write INHX16	*/
  {
    press_any();
    erase_err();
  }
}


/* -----------------------------------------------------------------------
   Select the parallel port /dev/lpX
   ----------------------------------------------------------------------- */
int select_port( int ch, int *port, int select ) {
  switch ( ch ) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    if( (ch - '0') <= max_lp ) {
      *port = ch - '0';
      if( !init_port() ) {
	*port = -1;
      }
    }
    break;

  case 0x0D:
  case 0x0A:
  case KEY_DOWN:
    if( select < 8 )
      select++;
    return select;
    break;
    		
  case KEY_UP:
    if( select > 0 )
      select--;
    return select;
    break;
    		
  default:	return 0;
    break;
  } /* switch(ch) */

  return 0;
}


/* -----------------------------------------------------------------------
   Select the address -- hex, 4 digits max.
   ----------------------------------------------------------------------- */
int select_addr( int ch, int *addr, int select )
{
  int	pos = 0, done = 0;
  char	value[5];
  
  /* Convert the current value to a string */
  sprintf( value, "%04X", *addr );

  while ( !done )
  {  
    switch ( ch )
    {
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
      case 0x1B:  return -1;
      		  break;
      		  
      case 's':
      case 'S':   mvaddstr( 3, 5, "Configuration written to ~/.picprgrc                   " );
                  move( 5+select, 56+pos );
                  if ( write_config() == FALSE )
      		  {
      		    press_any();
      		    erase_err();
      		  }
      		  break;

      case 0x0D:
      case 0x0A:
      case KEY_DOWN:
                  if( select < 8 )
                    select++;
      		  return select;
      		  break;

      case KEY_UP:
                  if( select > 0 )
                    select--;
    		  return select;
    		  break;
    		    		
      case KEY_RIGHT:  if ( pos < 3 ) pos++;
      		       move( 5+select, 56+pos );
      		       break;
    		  		  
      case KEY_LEFT:   if ( pos > 0 ) pos--;
      		       move( 5+select, 56+pos );
		       break;
		       
      default:	       if ( isxdigit( ch ) )
                       {
                         value[pos] = toupper(ch);
                         if ( pos < 3 ) pos++;
                         addch( toupper(ch) );
                         move( 5+select, 56+pos );
                         sscanf( value, "%x", addr );
                       }
    		       break;
    		       		  
    } /* switch */
    refresh();
    ch = getch();
  } /* which */
  
  return select;
}


/* -----------------------------------------------------------------------
   Select the pin number 1-25
   ----------------------------------------------------------------------- */
int select_pin( int ch, int *pin, int *pol, int *state, int select )
{
  int	pos = 0, done = 0, tmp_pin;
  char	pin_ch[3];
  
  sprintf( pin_ch, "%02d", *pin );

  while ( !done )
  {  
    switch ( ch )
    {
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
      case 0x1B:  return -1;
      		  break;
      		  
      case 's':
      case 'S':   mvaddstr( 3, 5, "Configuration written to ~/.picprgrc                   " );
                  move( 5+select, 56+pos );
                  if ( write_config() == FALSE )
      		  {
      		    press_any();
      		    erase_err();
      		  }
      		  break;

      /* Exit and return the next selection number */
      case 0x0D:
      case 0x0A:
      case KEY_DOWN:
                  if( select < 8 )		/* 7 is the last one 	*/
      		    select++;
      		  return select;
      		  break;

      /* Exit and return the previous selection */
      case KEY_UP:
                  if( select > 0 )
                    select--;
    		  return select;
    		  break;
    		    		
      case KEY_RIGHT:  if ( pos == 0 )
                       {
                         pos = 1;
      		         move( 5+select, 56+pos );
      		       }
      		       break;
    		  		  
      case KEY_LEFT:   if ( pos > 0 )
                       {
                         pos = 0;
      		         move( 5+select, 56+pos );
      		       }
		       break;

      case '-':        *pol = 0;
                       move( 5+select, 54+pos );
                       addch( '-' );
                       move( 5+select, 56+pos );
                       break;
                       
                       
      case '+':        *pol = 1;
                       move( 5+select, 54+pos );
                       addch( '+' );
                       move( 5+select, 56+pos );
                       break;

      /* Turn on the selected bit, using current setting */
      case 'f':
      case 'F':        if( state != NULL )
                       {
                         switch( select )
                         {
                           case 1:	vppoff();
                         		break;
                         		
                           case 2:	vddoff();
                         		break;
                         		
                           case 3:	clklo();
                         		break;
                         
                           case 4:	outlo();
                           		mvaddstr( 6+select, 62, 
                           		          inbit() ? "on " : "off" );
                         		break;
                         		
                           default:	break;
                         }
                         *state = 0;
                         mvaddstr( 5+select, 62, "off" );
                         move( 5+select, 56+pos );
                       }
                       break;
      
      /* Turn off the selected bit, using current setting */
      case 'o':
      case 'O':        if( state != NULL )
                       {
                         switch( select )
                         {
                           case 1:	vppon();
                         		break;
                         		
                           case 2:	vddon();
                         		break;
                         		
                           case 3:	clkhi();
                         		break;
                         		
                           case 4:	outhi();
                           		mvaddstr( 6+select, 62, 
                           		          inbit() ? "on " : "off" );
                         		break;
                         
                           default:	break;
                         }
                         *state = 1;
                         mvaddstr( 5+select, 62, "on " );
                         move( 5+select, 56+pos );
                       }
                       break;

      default:	       if ( isdigit( ch ) )
                       {
                         pin_ch[pos] = ch;
                         if ( pos == 0 )
                           pos++;
                         addch( ch );
                         move( 5+select, 56+pos );
                         /* Check for a valid pin # first */
                         sscanf( pin_ch, "%02d", &tmp_pin );
                         if( tmp_pin < 18 )
                           *pin = tmp_pin;
                       }
    		       break;
    		       		  
    } /* switch */

    /* Update the display with the new info */
    refresh();
    
    /* Get the next key */
    ch = getch();
  } /* which */
  
  return select;
}


/* -----------------------------------------------------------------------
   Configure the programmer -- Programmer type, ID, EEPROM locations, port
   
   Allow the user to specify the DB-25 pin number for each pin and the
   logic state for its on state.
   
     VPP control
     Vdd control
     Clock to PIC
     Data to PIC
     Data from PIC
   
   Display help for the line that you are on.
   
   ----------------------------------------------------------------------- */
void config()
{
  int	ch, select=0, done=0, line, indent,
    vpp_state=0,
    vdd_state=0,
    clk_state=0,
  out_state=0;
  
  /* Clear the screen and show the title bar */
  title();

  while ( !done )
  {
    line = 5;
    indent = 15;

    /* Select port 0-3 or whatever are recognized */
    mvprintw( line++, indent,
              "Printer Port selection [%d-%d]         :  [%d]",
              0, max_lp,
              pconfig.port
            );

    /* Show the current Pin settings */
    mvprintw( line++, indent,
              "Vpp control pin #                    : %c[%02d]   %s ",
              pconfig.VPP_pol ? '+' : '-',
              pconfig.VPP_pin,
              vpp_state ? "on" : "off"
            );

    mvprintw( line++, indent,
              "Vdd control pin #                    : %c[%02d]   %s ",
              pconfig.VDD_pol ? '+' : '-',
              pconfig.VDD_pin,
              vdd_state ? "on" : "off"
            );
              
    mvprintw( line++, indent,
              "Clock pin #                          : %c[%02d]   %s ",
              pconfig.CLK_pol ? '+' : '-',
              pconfig.CLK_pin,
              clk_state ? "on" : "off"
            );

    mvprintw( line++, indent,
              "Data to PIC pin #                    : %c[%02d]   %s ",
              pconfig.DATAO_pol ? '+' : '-',
              pconfig.DATAO_pin,
              out_state ? "on" : "off"
            );

    mvprintw( line++, indent,
              "Data from PIC pin #                  : %c[%02d]   %s ",
              pconfig.DATAI_pol ? '+' : '-',
              pconfig.DATAI_pin,
              inbit() ? "on" : "off"
            );

    /* Select start memory locations ID/FUSES, EEPROM */  
    mvprintw( line++, indent,
              "ID memory location in HEX file (HEX) :  [%04X]",
              pconfig.IDaddr
            );
    
    mvprintw( line++, indent,
              "EEPROM memory location in file (HEX) :  [%04X]",
              pconfig.EEaddr
            );

    mvprintw( line++, indent,
              "Config FUSES location in file (HEX)  :  [%04X]",
              pconfig.CFaddr
            );

    /* Show special options */
    attrset( COLOR_PAIR( 5 ) );
    switch( select )
    {
      case 0:	mvaddstr( 15, 5, "Enter parallel port 0-2                                      " );
                mvaddstr( 16, 5, "                                            ");
      		break;
      		
      case 1:	mvaddstr( 15, 5, "Enter VPP pin number and select polarity with +/-           " );
                mvaddstr( 16, 5, "Use [O]n and o[F]f to toggle the output line");
		break;
		
      case 2:	mvaddstr( 15, 5, "Enter Vdd pin number and select polarity with +/-            " );
                mvaddstr( 16, 5, "Use [O]n and o[F]f to toggle the output line");
		break;
		
      case 3:	mvaddstr( 15, 5, "Enter Clock pin number and select polarity with +/-          " );
                mvaddstr( 16, 5, "Use [O]n and o[F]f to toggle the output line");
		break;
		
      case 4:	mvaddstr( 15, 5, "Enter Data to PIC pin number and select polarity with +/-    " );
                mvaddstr( 16, 5, "Use [O]n and o[F]f to toggle the output line");
		break;

      case 5:	mvaddstr( 15, 5, "Enter Data from PIC pin number and select polarity with +/-  " );
                mvaddstr( 16, 5, "                                            ");
		break;
		
      case 6:	mvaddstr( 15, 5, "Enter starting address in HEX                                " );
      		break;
      		
      case 7:	mvaddstr( 15, 5, "Enter starting address in HEX                                " );
      		break;

      case 8:	mvaddstr( 15, 5, "Enter starting address in HEX                                " );
      		break;
      		
      default:
      		break;
    }
    
    attrset( COLOR_PAIR( 3 ) );
    mvaddstr( lastline, 0, "  Arrow keys to move          [S]ave configuration         [Q]uit to main menu  " );
    attrset( COLOR_PAIR( 1 ) );

    /* Move the cursor to the line being edited */
    move( 5+select, 41+indent );
    refresh();
  
    /* Get keypresses. Q quits as does ESC and X. Arrows move directions */
    ch = getch();

    /* Check for exit keys first */
    switch( ch )
    {
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
      case 0x1B:  done = 1;
      		  break;
 		  
      case 's':
      case 'S':   mvaddstr( 3, 5, "Configuration written to ~/.picprgrc                   " );
                  if ( write_config() == FALSE )
      		  {
      		    press_any();
      		    erase_err();
      		  }
      		  break;

      default:
        switch( select )
        {
          /* Selecting the Parallel port -- Allow numbers */
          case 0:  select = select_port( ch, &pconfig.port, select );
    	  	   break;

          /* Select a pin numer and polarity -- allow +/- */
          case 1:  select = select_pin( ch, &pconfig.VPP_pin,
                                            &pconfig.VPP_pol,
                                            &vpp_state,
                                            select );
    		   break;

          case 2:  select = select_pin( ch, &pconfig.VDD_pin,
                                            &pconfig.VDD_pol,
                                            &vdd_state,
                                            select );
    		   break;

          case 3:  select = select_pin( ch, &pconfig.CLK_pin,
                                            &pconfig.CLK_pol,
                                            &clk_state,
                                            select );
    		   break;

          case 4:  select = select_pin( ch, &pconfig.DATAO_pin,
                                            &pconfig.DATAO_pol,
                                            &out_state,
                                            select );
    		   break;

          case 5:  select = select_pin( ch, &pconfig.DATAI_pin,
                                            &pconfig.DATAI_pol,
                                            NULL,
                                            select );
    		   break;

          /* Selecting memory locations -- Allow 0-9,A-F */
          case 6:  select = select_addr( ch, &pconfig.IDaddr, select );
    		   break;
    		   
    	  case 7:  select = select_addr( ch, &pconfig.EEaddr, select );
    	  	   break;

    	  case 8:  select = select_addr( ch, &pconfig.CFaddr, select );
    	  	   break;
    		
          default:
    		   break;
        } /* Switch selection */
    } /* Switch ch */

    /* If the select routine wants and exit, give it to them */
    if ( select == -1 )
      done = 1;
  } /* While */

  /* Make sure everything is off when we leave */
  vppoff();
  vddoff();
  clklo();
  outlo();
}


/* -----------------------------------------------------------------------
   Prompt for the fill value for memory. Default to 0x03FF
   ----------------------------------------------------------------------- */
void fill_mem()
{

}


/* -----------------------------------------------------------------------
   Main Menu display. Gets a keypress...
   ----------------------------------------------------------------------- */
int main_menu()
{
  title();

  mvaddstr( 6, 27, "[L]oad a file" );
  mvaddstr( 7, 27, "[W]rite RAM to file");
  mvaddstr( 8, 27, "[D]isply RAM" );
  mvaddstr( 9, 27, "[F]ill RAM with blank state (0xFFFF)" );

  mvaddstr( 11, 27, "[R]ead contents of PIC into RAM" );
  mvaddstr( 12, 27, "[P]rogram PIC from RAM");
  mvaddstr( 13, 27, "[V]erify PIC with RAM" );

  mvaddstr( 15, 27, "[C]onfigure PICPRG");
  mvaddstr( 16, 27, "[Q]uit");
  move( 0, 0 );
  
  refresh();

  return getch();
}

/* -----------------------------------------------------------------------
   Setup my color pairs
   
   Make these user defined eventually
   ----------------------------------------------------------------------- */ 
void setup_colors()
{
  init_pair( 1, COLOR_WHITE, COLOR_BLUE );
  init_pair( 2, COLOR_WHITE, COLOR_RED );
  init_pair( 3, COLOR_BLACK, COLOR_WHITE );
  init_pair( 4, COLOR_BLUE, COLOR_WHITE );
  init_pair( 5, COLOR_CYAN, COLOR_BLUE );
  init_pair( 6, COLOR_RED, COLOR_WHITE );
}

void setup_mono()
{
  init_pair( 1, COLOR_WHITE, COLOR_BLACK );
  init_pair( 2, COLOR_WHITE, COLOR_BLACK );
  init_pair( 3, COLOR_BLACK, COLOR_WHITE );
  init_pair( 4, COLOR_BLACK, COLOR_WHITE );
  init_pair( 5, COLOR_WHITE, COLOR_BLACK );
  init_pair( 6, COLOR_BLACK, COLOR_WHITE );
}



/* -----------------------------------------------------------------------
   Main code
   ----------------------------------------------------------------------- */ 
void usage( char *prog_name ) {
  printf("\n Linux PIC84 programmer     v%s         Copyright 1994-2002 by Brian C. Lane ", VERSION );
  printf("\nGNU Public License v2.0 - http://www.brianlane.com");
  printf("\nUsage: %s -p[0-2] -i[hex] -e[hex] <filename>\n\n", prog_name );
  printf("  -p[0-2]  =  Parallel port to use\n");
  printf("  -i[hex]  =  HEX value of ID data in memory map\n");
  printf("  -e[hex]  =  HEX value of EEPROM data in memory map\n");
  printf("  -v       =  Verify after programming (command line mode)\n");
  printf("  -q       =  Quiet mode (command line mode)\n");
  printf("  -d       =  Debug mode\n");
  printf("\n\n");
}


/* -----------------------------------------------------------------------
   Main code
   ----------------------------------------------------------------------- */ 
int main( int argc, char *argv[] )
{
  int 	done = 0,
        status = 0;
  char	ch;
  WINDOW *scrollw;

  /* Read configuration file from ~/.picprgrc */
  read_config();

  /* Clear our PIC memory storage area */
  init_pic();

  /* process command line arguments -- Override config and file settings */
  ch = 0;
  while ( ch != -1 )
  {
    ch = getopt( argc, argv, "?vqmp:i:e:t:d" );
    
    switch ( ch )
    {
      /* Quiet mode for command line operation */
      case 'q':
	        options |= OPT_QUIET;
		break;

      /* Verify after programming in command line operation */
      case 'v':
	        options |= OPT_VERIFY;
		break;

      /* Turn on debugging info */
      case 'd':
      		options |= OPT_DEBUG;
      		break;
      		
      /* Set the port to 0-2 */
      case 'p':
      		pconfig.port = atoi( optarg );
      		break;

      /* Set the ID address in hex */      		
      case 'i':
      		sscanf( optarg, "%x", &pconfig.IDaddr );
      		break;
      		
      /* Set the EEPROM address in hex */
      case 'e':
      		sscanf( optarg, "%x", &pconfig.EEaddr );
      		break;
      		
      case '?':
	usage( argv[0] );
      		return 0;
      		break;
      		
      case 'm':
      		pconfig.colors = FALSE;
      		break;
      		
      default:	break;
      
    } /* Switch */
  } /* While */

  /* Get the base addresses of the parallel ports */
  init_lp_table();

  /* pass lp number on command line */
  if ( !init_port() )
  {
    fprintf( stderr, "Failed to initalize /dev/lp%d\n", pconfig.port );
    exit( -1 );
  }

  /* Initalize all the control lines to off */
  vppoff();
  vddoff();
  clklo();
  outlo();

  if ( optind < argc )
  {
    /* Load the file from disk */
    if ( load_hex( argv[optind] ) == FALSE )
    {
      fprintf( stderr, "Error loading %s\n", argv[optind] );
      exit( -1 );
    }

    /* Program the device, show errors on stderr */
    if( program_pic( NULL ) == 0 )
      {

	/* If the user selected verify, then verify it too and return -1 for bad and 0 for ok */
	if( options & OPT_VERIFY )
	  status = verify_pic( NULL );
	else
	  status = 0;
      } else {
	status = -1;
      }

    /* Make sure everything is off when we leave */
    vppoff();
    vddoff();
    clklo();
    outlo();

    exit( status );
  }


  /* Initalize Ncurses and color if available */
  if ( initscr() == NULL  )
  {
    pconfig.colors = FALSE;
  }

  if( pconfig.colors == TRUE )
  {
    if( has_colors() )
    {
      start_color();
      setup_colors();
    }
  } else {
    setup_mono();
  }
  
  noecho();
  cbreak();  
  keypad( stdscr, TRUE );
  

  /* If no valid command line parameters are passed, show the main menu */
  while( !done )
  {
    switch( toupper( main_menu() ) )
    {
      case 'P' :  title();
      		  mvaddstr( 2, 30, "Programming PIC" );
      		  refresh();
      		  scrollw = newwin( 0, 0, 4, 0 );
                  attr_clear( scrollw, 80, 23, COLOR_PAIR( 1 ) );
/*                  touchwin( scrollw ); */
		  refresh();
		  scrollok( scrollw, TRUE );
                  program_pic( scrollw );
		  press_any();
                  delwin( scrollw );
                  touchwin( stdscr );
      		  break;

      case 'R' :  mvaddstr( 2, 32, "Reading PIC" );
      		  refresh();
      		  init_pic();
                  read_pic( stdscr );
                  break;
                  
      case 'D' :  edit_pic();
      		  break;
      
      /* Clear our PIC memory storage area */		  
      case 'F' :  attrset( COLOR_PAIR( 5 ) );
                  mvaddstr( 18, 15, "Are you sure you want to erase RAM?");
                  ch = getch();
                  if( toupper( ch ) == 'Y' )
                  init_pic();
                  attrset( COLOR_PAIR( 1 ) );
                  mvaddstr( 17, 15, "                                   ");
                  break;
      		  
      case 'L' :  load_file();
      		  break;
      		  
      case 'V' :  title();
                  mvaddstr( 2, 30, "Verifying PIC" );
                  refresh();
      		  scrollw = newwin( 0, 0, 4, 0 );
                  attr_clear( scrollw, 80, 23, COLOR_PAIR( 1 ) );
/*                  touchwin( scrollw ); */
		  refresh();
		  scrollok( scrollw, TRUE );
                  verify_pic( scrollw );
		  press_any();
                  delwin( scrollw );
                  touchwin( stdscr );
      		  break;
      		  
      case 'W' :  write_file();
      		  break;

      case 'C' :  config();
      		  break;
      		              
      case 'Q' :  done = 1;
      		  break;

      default: break;
    }
  }

  /* Make sure everything is off when we leave */
  vppoff();
  vddoff();
  clklo();
  outlo();

  endwin();
  
  return 0;
}
