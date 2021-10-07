/* Copyright 2021 Rafał Jopek ( rafaljopek at hotmail com )
 *
 * he.prg
 *
 * Simple program editor in Harbour.
 *
 * Compile:    hbmk2 he.hbp
 * Execute:    he <file>
 *
 */

#include "box.ch"
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

STATIC nWRow   := 0                                 /* specifies the row, col visible in the window */
STATIC nWCol   := 0
STATIC nAddRow := 0
STATIC nAddCol := 0

PROCEDURE Main( cFileName )

   LOCAL aText := {}

   LOCAL i
   LOCAL nChoice
   LOCAL nKey
   LOCAL nKeyStd
   LOCAL nLeft
   LOCAL nLine
   LOCAL nMaxCol := 0
   LOCAL nMaxRow := 0
   LOCAL nVLine := 1                                /* vertical line */

   LOCAL cString
   LOCAL cSubString

   LOCAL lApp_continue := .T.
   LOCAL lEditor_cursorStyle := .F.
   LOCAL lEditor_lineNumbers := .T.
   LOCAL lEditor_renderLineHighlight := .T.
   LOCAL lFiles_autoSave := .F.

   Scroll()

   Set( _SET_SCOREBOARD, .F. )
   Set( _SET_EVENTMASK, hb_bitOr( INKEY_KEYBOARD, HB_INKEY_GTEVENT, INKEY_ALL ) )
   Set( _SET_INSERT, .T. )

   /* Setup input CP of the translation */
   hb_cdpSelect( "UTF8EX" )
   hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
   hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )

   /* Configure terminal and OS codepage */
   hb_SetTermCP( hb_cdpTerm() )
   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
   hb_gtInfo( HB_GTI_ICONFILE, "docs/assets/img/harbour.icns" )

   IF cFileName == NIL
      cFileName := "Untitled-1.prg"
      AAdd( aText, "" )
   ELSE
      IF File( cFileName )
         aText := hb_ATokens( hb_MemoRead( cFileName ), .T. )
      ELSE

         nChoice := hb_Alert( "Cannot find the file " + '"' + cFileName + '";' + ";" + ;
            "Do you want to create a new file?", { "Yes", "No", "Cancel" }, 0xf0 )

         SWITCH nChoice
         CASE 1
            aText := hb_ATokens( hb_MemoRead( cFileName ), .T. )
            EXIT

         CASE 2
            cFileName := "Untitled-1.prg"
            AAdd( aText, "" )
            EXIT

         CASE 3
            QUIT

         OTHERWISE
            QUIT

         ENDSWITCH

      ENDIF
   ENDIF

   hb_gtInfo( HB_GTI_WINTITLE, cFileName )

   DO WHILE lApp_continue

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      Scroll()
      DispBegin()

      hb_DispBox( 0, 0, nMaxRow, nMaxCol, "         ", 0x0 )

      /* Write a value to the display */
      FOR i := 0 TO nMaxRow

         nLine := i + nAddRow + 1

         IF nLine <= Len( aText )

            /* Find the longest string element */
            nLeft := Len( hb_ntos( Len( aText ) ) )

            /* Controls the display of line numbers. */
            IF lEditor_lineNumbers
               hb_DispOutAt(  i, 0, PadL( hb_ntos( nLine ), nLeft ), iif( i == nWRow, "15/00", "07/00" ) )
            ENDIF

            hb_DispOutAt( i, nLeft, "▕", "08/00")

            hb_DispOutAt( i, nLeft + nVLine, PadR( aText[ nLine ], nMaxCol + 1 ), if( lEditor_renderLineHighlight, iif( i == nWRow, "00/15", "07/00" ), NIL ) )
         ELSE
            Scroll( i, 0, nMaxRow, nMaxCol )
            hb_DispOutAt( i, 0, ">> EOF <<", 0x01 )
            EXIT
         ENDIF

      NEXT

      DispEnd()

      SetPos( nWRow, nLeft + nVLine + nWCol )

      nKey := Inkey( 0 )

      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC                                    /*   Esc, Ctrl-[ */
         lApp_continue := .F.

         EXIT

      CASE K_LBUTTONDOWN                            /*   Mouse left button down */

         IF nWRow + 1 <= Len( aText ) .AND. MRow() + 1 <= Len( aText )

            nWRow := MRow()
            nWCol := MCol()

            SetPosCol( aText )

         ENDIF

         EXIT

      CASE K_RBUTTONUP

         EXIT

      CASE K_MWFORWARD                              /*   Mouse Wheel Forward */

         IF nWRow > 0
            nWRow--
         ELSE
            IF nAddRow > 0
               nAddRow--
            ENDIF
         ENDIF

         SetPosCol( aText )

         EXIT

      CASE K_MWBACKWARD                             /*   Mouse Wheel Backward */

         SetPosRow( aText, nMaxRow )

         SetPosCol( aText )

         EXIT

      CASE K_UP                                     /*   Up arrow, Ctrl-E */

         IF nWRow > 0
            nWRow--
         ELSE
            IF nAddRow > 0
               nAddRow--
            ENDIF
         ENDIF

         SetPosCol( aText )

         EXIT

      CASE K_LEFT                                   /*   Left arrow, Ctrl-S */

         IF nWCol > 0
            nWCol--
         ELSE
            IF nAddCol > 0
               nAddCol--
            ENDIF
         ENDIF

         EXIT

      CASE K_DOWN                                   /*   Down arrow, Ctrl-X */

         SetPosRow( aText, nMaxRow )

         SetPosCol( aText )

         EXIT

      CASE K_RIGHT                                  /*   Right arrow, Ctrl-D */

         IF nWCol < Len( aText[ nAddRow + nWRow + 1 ] )
            nWCol++
         ENDIF

         EXIT

      CASE K_HOME                                   /*   Home, Ctrl-A */

         nWCol := 0

         EXIT

      CASE K_END                                    /*   End, Ctrl-F */

         nWCol := Len( aText[ nAddRow + nWRow + 1 ] )

         EXIT

      CASE K_PGUP                                   /*   PgUp, Ctrl-R */

         IF nWRow <= 0
            IF nAddRow - nMaxRow  >= 0
               nAddRow -= nMaxRow
            ENDIF
         ENDIF
         nWRow := 0

         EXIT

      CASE K_PGDN                                   /*   PgDn, Ctrl-C */

         IF nWRow >= nMaxRow
            IF nAddRow +  nMaxRow <= Len( aText )
               nAddRow += nMaxRow
            ENDIF
         ENDIF

         nWRow := Min( nMaxRow, Len( aText ) - nAddRow - 1 )

         Scroll( 0, 0, nMaxRow, nMaxCol )

         EXIT

      CASE K_ENTER                                  /*   Enter, Ctrl-M */

         IF aText[ nAddRow + nWRow + 1 ] == "" .AND. nWCol == 0

            hb_AIns( aText, nAddRow + nWRow + 1, "", .T. )

            SetPosRow( aText, nMaxRow )

         ELSE
            IF nWCol == Len( aText[ nAddRow + nWRow + 1 ] )
               hb_AIns( aText, nAddRow + nWRow + 2, "", .T. )
               nWRow++ //
               nWCol := 0
            ELSE
               cSubString := Right( aText[ nAddRow + nWRow + 1 ], Len( aText[ nAddRow + nWRow + 1 ] ) - nWCol )

               cString := aText[ nAddRow + nWRow + 1 ]

               aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, Len( aText[ nAddRow + nWRow + 1 ] ) - nWCol, "" )

               hb_AIns( aText, nAddRow + nWRow + 2, cSubString, .T. )

               SetPosRow( aText, nMaxRow )

               nWCol := 0

            ENDIF
         ENDIF

         EXIT

      CASE K_INS                                    /*   Ins, Ctrl-V */

         IF lEditor_cursorStyle
            SetCursor( SC_NORMAL )
            lEditor_cursorStyle := .F.
         ELSE
            SetCursor( SC_INSERT )
            lEditor_cursorStyle := .T.
         ENDIF

         EXIT

      CASE K_DEL                                    /*   Del, Ctrl-G */

         IF aText[ nAddRow + nWRow + 1 ] == ""
            IF nWRow >= 0
               hb_ADel( aText, nAddRow + nWRow + 1, .T. )
            ENDIF
         ELSE
            IF nWCol == Len( aText[ nAddRow + nWRow + 1 ] )

               aText[ nAddRow + nWRow + 1 ] += aText[ nAddRow + nWRow + 1 + 1 ]

               hb_ADel( aText, nAddRow + nWRow + 1 + 1, .T. )
            ELSE
               cString := aText[ nAddRow + nWRow + 1 ]
               aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, 1, "" )
            ENDIF
         ENDIF

         EXIT

      CASE K_BS                                     /*   Backspace, Ctrl-H */

         IF aText[ nAddRow + nWRow + 1 ] == ""
            IF nWRow > 0
               hb_ADel( aText, nAddRow + nWRow + 1, .T. )
               nWRow--
               nWCol := Len( aText[ nAddRow + nWRow + 1 ] )
            ENDIF
         ELSE

            IF nWCol > 0
               cString := aText[ nAddRow + nWRow + 1 ]
               aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol, 1, "" )
               nWCol--
            ELSE

               IF nWRow > 0
                  IF aText[ nAddRow + nWRow + 1 - 1 ] == ""
                     nWCol := 0
                  ELSE
                     nWCol := Len( aText[ nAddRow + nWRow + 1 - 1 ] )
                  ENDIF

                  aText[ nAddRow + nWRow + 1 - 1 ] += aText[ nAddRow + nWRow + 1 ]

                  hb_ADel( aText, nAddRow + nWRow + 1, .T. )
                  nWRow--
               ENDIF
            ENDIF
         ENDIF

         EXIT

      CASE K_TAB                                    /*   Tab, Ctrl-I */
         cString := aText[ nAddRow + nWRow + 1 ]
         aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lEditor_cursorStyle, 1, 0 ), "   " )

         nWCol += 3

         EXIT

      CASE K_F1                                     /*   F1, Ctrl-Backslash */

         EXIT

      CASE K_SH_F10                                 /*   Shift-F10 */

         EXIT

      CASE HB_K_RESIZE

         EXIT

      OTHERWISE

         IF ( nKeyStd >= 32 .AND. nKeyStd <= 126 ) .OR. ( nKeyStd >= 160 .AND. nKeyStd <= 255 ) .OR. ! hb_keyChar( nKeyStd ) == ""

            cString := aText[ nAddRow + nWRow + 1 ]

            aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lEditor_cursorStyle, 1, 0 ), AddChar( nKeyStd ) )

            nWCol++

         ENDIF

         IF lFiles_autoSave
            cString := ""
            AEval( aText, {| e | cString += e + hb_eol() } )
            hb_MemoWrit( cFileName, cString )
         ENDIF

      ENDSWITCH

   ENDDO

   RETURN

STATIC PROCEDURE SetPosRow( aText, nMaxRow )

   IF nWRow < nMaxRow .AND. nWRow + 1 < Len( aText )
      nWRow++
   ELSE
      IF nAddRow + nWRow + 1 < Len( aText )
         nAddRow++
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE SetPosCol( aText )

   IF aText[ nAddRow + nWRow + 1 ] == ""
      nWCol := 0
   ELSE
      IF nWCol > Len( aText[ nAddRow + nWRow + 1 ] )
         nWCol := Len( aText[ nAddRow + nWRow + 1 ] )
      ENDIF
   ENDIF

   RETURN

STATIC FUNCTION AddChar( nKeyStd )

   SWITCH nKeyStd
   CASE 34 // "
      nKeyStd := hb_keyChar( nKeyStd ) + hb_keyChar( 34 )
      EXIT

   CASE 39 // '
      nKeyStd := hb_keyChar( nKeyStd ) + hb_keyChar( 39 )
      EXIT

   CASE 40 // (
      nKeyStd := hb_keyChar( nKeyStd ) + hb_keyChar( 41 )
      EXIT

   CASE 91 // [
      nKeyStd := hb_keyChar( nKeyStd ) + hb_keyChar( 93 )
      EXIT

   CASE 123 // {
      nKeyStd := hb_keyChar( nKeyStd ) + hb_keyChar( 125 )
      EXIT

   OTHERWISE

      nKeyStd := hb_keyChar( nKeyStd )

   ENDSWITCH

   RETURN nKeyStd
