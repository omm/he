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

STATIC nAddCol := 0
STATIC nAddRow := 0
STATIC nWCol   := 0
STATIC nWRow   := 0                                 /* specifies the row, col visible in the window */

PROCEDURE Main( cFileName )

   LOCAL aText := {}

   LOCAL i
   LOCAL nChoice
   LOCAL nClipboardAction := 0
   LOCAL nKey
   LOCAL nLeft
   LOCAL nLine
   LOCAL nMaxCol := 0
   LOCAL nMaxRow := 0
   LOCAL nVLine := 1                                /* vertical line */

   LOCAL cClipboardAction := ""
   LOCAL cString
   LOCAL cSubString

   LOCAL lApp_Continue := .T.
   LOCAL lAutoSave := .T.
   LOCAL lCursorStyle := .F.
   LOCAL lLineNumbers := .T.
   LOCAL lRenderLineHighlight := .T.

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

         nChoice := hb_Alert( "Cannot find the file " + '"' + cFileName + '";' + ";" + "Do you want to create a new file?", { "Yes", "No", "Cancel" }, "00/15" )

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

   DO WHILE lApp_Continue

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      Scroll()

      hb_DispBox( 0, 0, nMaxRow, nMaxCol, "         ", "00/00" )

      DispBegin()
      /* Write a value to the display */
      FOR i := 0 TO nMaxRow

         nLine := i + nAddRow + 1

         IF nLine <= Len( aText )

            /* Find the longest string element */
            nLeft := Len( hb_ntos( Len( aText ) ) )

            /* Controls the display of line numbers */
            IF lLineNumbers
               hb_DispOutAt(  i, 0, PadL( hb_ntos( nLine ), nLeft ), iif( i == nWRow, "15/00", "07/00" ) )
            ENDIF

            hb_DispOutAt( i, nLeft, "▕", "08/00" ) /* (U+2595) */

            hb_DispOutAt( i, nLeft + nVLine, PadR( aText[ nLine ], nMaxCol + 1 ), SyntaxColoring( nLine, nClipboardAction, lRenderLineHighlight, i, nWRow ) )
         ELSE
            Scroll( i, 0, nMaxRow, nMaxCol )
            hb_DispOutAt( i, 0, ">> EOF <<", "01/00" )
            EXIT
         ENDIF

      NEXT

      DispEnd()

      SetPos( nWRow, nLeft + nVLine + nWCol )

      /* Extract a character from the keyboard buffer or a mouse event */
      nKey := Inkey( 0 )

      /* Key Shift */
      IF hb_gtInfo( HB_GTI_KBDSHIFTS ) == hb_bitOr( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT )

         SWITCH nKey
         CASE K_UP
            EXIT
         CASE K_DOWN
            EXIT
         CASE K_LEFT
            EXIT
         CASE K_RIGHT
            EXIT
         CASE K_HOME
            EXIT
         CASE K_END
            EXIT
         CASE K_PGUP
            EXIT
         CASE K_PGDN
            EXIT

         OTHERWISE

            IF ( nKey >= 32 .AND. nKey <= 126 ) .OR. ( nKey >= 160 .AND. nKey <= 255 ) .OR. ! hb_keyChar( nKey ) == ""

               cString := aText[ nAddRow + nWRow + 1 ]

               aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lCursorStyle, 1, 0 ), AddCharTwin( nKey ) )

               nWCol++

            ENDIF

            IF lAutoSave
               cString := ""
               AEval( aText, {| e | cString += e + hb_eol() } )
               hb_MemoWrit( cFileName, cString )
            ENDIF

         ENDSWITCH
         LOOP
      ENDIF

      /* Key Ctrl */
      IF hb_gtInfo( HB_GTI_KBDSHIFTS ) == hb_bitOr( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_CTRL )
         SWITCH nKey
         CASE K_CTRL_C

            cClipboardAction := aText[ nAddRow + nWRow + 1 ]
            nClipboardAction := nAddRow + nWRow + 1
            EXIT

         CASE K_CTRL_V

            IF nWCol == Len( aText[ nAddRow + nWRow + 1 ] )
               hb_AIns( aText, nAddRow + nWRow + 2, cClipboardAction, .T. )
               nWRow++
               nWCol := Len( aText[ nAddRow + nWRow + 1 ] )
            ELSE
               hb_AIns( aText, nAddRow + nWRow + 2, cClipboardAction, .T. )
               nWRow++
               nWCol := 0
            ENDIF

            nClipboardAction := 0

            EXIT

         CASE K_CTRL_X

            cClipboardAction := aText[ nAddRow + nWRow + 1 ]
            hb_ADel( aText, nAddRow + nWRow + 1, .T. )
            nWCol := 0

            EXIT

         ENDSWITCH
         LOOP
      ENDIF

      SWITCH nKey

      CASE K_ESC
         lApp_continue := .F.

         EXIT

      CASE K_LBUTTONDOWN

         IF nWRow + 1 <= Len( aText ) .AND. MRow() + 1 <= Len( aText )

            nWRow := MRow()
            nWCol := MCol() - nLeft - nVLine

            SetPosCol( aText )

         ENDIF

         EXIT

      CASE K_RBUTTONUP

         EXIT

      CASE K_MWFORWARD

         IF nWRow > 0
            nWRow--
         ELSE
            IF nAddRow > 0
               nAddRow--
            ENDIF
         ENDIF

         SetPosCol( aText )

         EXIT

      CASE K_MWBACKWARD

         SetPosRow( aText, nMaxRow )

         SetPosCol( aText )

         EXIT

      CASE K_UP

         IF nWRow > 0
            nWRow--
         ELSE
            IF nAddRow > 0
               nAddRow--
            ENDIF
         ENDIF

         SetPosCol( aText )

         EXIT

      CASE K_LEFT

         IF nWCol > 0
            nWCol--
         ELSE
            IF nAddCol > 0
               nAddCol--
            ENDIF
         ENDIF

         EXIT

      CASE K_DOWN

         SetPosRow( aText, nMaxRow )

         SetPosCol( aText )

         EXIT

      CASE K_RIGHT

         IF nWCol < Len( aText[ nAddRow + nWRow + 1 ] )
            nWCol++
         ENDIF

         EXIT

      CASE K_HOME

         nWCol := 0

         EXIT

      CASE K_END

         nWCol := Len( aText[ nAddRow + nWRow + 1 ] )

         EXIT

      CASE K_PGUP

         IF nWRow <= 0
            IF nAddRow - nMaxRow  >= 0
               nAddRow -= nMaxRow
            ENDIF
         ENDIF
         nWRow := 0

         EXIT

      CASE K_PGDN

         IF nWRow >= nMaxRow
            IF nAddRow +  nMaxRow <= Len( aText )
               nAddRow += nMaxRow
            ENDIF
         ENDIF

         nWRow := Min( nMaxRow, Len( aText ) - nAddRow - 1 )

         Scroll( 0, 0, nMaxRow, nMaxCol )

         EXIT

         CASE K_ENTER

         IF aText[ nAddRow + nWRow + 1 ] == "" .AND. nWCol == 0

            hb_AIns( aText, nAddRow + nWRow + 1, "", .T. )
            SetPosRow( aText, nMaxRow )

         ELSE
            IF nWCol == Len( aText[ nAddRow + nWRow + 1 ] )
               hb_AIns( aText, nAddRow + nWRow + 2, "", .T. )
               nWRow++
               nWCol := 0
            ELSE
               /* Return a substring beginning with the rightmost character */
               cSubString := Right( aText[ nAddRow + nWRow + 1 ], Len( aText[ nAddRow + nWRow + 1 ] ) - nWCol )

               cString := aText[ nAddRow + nWRow + 1 ]

               aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, Len( aText[ nAddRow + nWRow + 1 ] ) - nWCol, "" )

               hb_AIns( aText, nAddRow + nWRow + 2, cSubString, .T. )

               SetPosRow( aText, nMaxRow )

               nWCol := 0

            ENDIF
         ENDIF

         EXIT

      CASE K_INS

         IF lCursorStyle
            SetCursor( SC_NORMAL )
            lCursorStyle := .F.
         ELSE
            SetCursor( SC_INSERT )
            lCursorStyle := .T.
         ENDIF

         EXIT

      CASE K_DEL

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

      CASE K_BS

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

      CASE K_TAB
         cString := aText[ nAddRow + nWRow + 1 ]
         aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lCursorStyle, 1, 0 ), "   " )

         nWCol += 3

         EXIT

      CASE K_F1

         EXIT

      CASE K_SH_F10

         EXIT

      CASE HB_K_RESIZE

         EXIT

      OTHERWISE

         IF ( nKey >= 32 .AND. nKey <= 126 ) .OR. ( nKey >= 160 .AND. nKey <= 255 ) .OR. ! hb_keyChar( nKey ) == ""

            cString := aText[ nAddRow + nWRow + 1 ]

            aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lCursorStyle, 1, 0 ), AddCharTwin( nKey ) )

            nWCol++

         ENDIF

         IF lAutoSave
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

STATIC FUNCTION AddCharTwin( nKey )

   LOCAL cChar

   SWITCH nKey
   CASE 34 // ""
      cChar := hb_keyChar( nKey ) + hb_keyChar( 34 )
      EXIT

   CASE 39 // ''
      cChar := hb_keyChar( nKey ) + hb_keyChar( 39 )
      EXIT

   CASE 40 // ()
      cChar := hb_keyChar( nKey ) + hb_keyChar( 41 )
      EXIT

   CASE 91 // []
      cChar := hb_keyChar( nKey ) + hb_keyChar( 93 )
      EXIT

   CASE 123 // {}
      cChar := hb_keyChar( nKey ) + hb_keyChar( 125 )
      EXIT

   OTHERWISE

      cChar := hb_keyChar( nKey )

   ENDSWITCH

   RETURN cChar

STATIC FUNCTION SyntaxColoring( nLine, nClipboardAction, lRenderLineHighlight, i, nWRow )

   LOCAL cColor

   IF nLine == nClipboardAction
      cColor := "04/00"
   ELSE
      IF lRenderLineHighlight
         IF i == nWRow
            cColor := "15/08"
         ELSE
            cColor := "07/00"
         ENDIF
      ELSE
         cColor := NIL
      ENDIF
   ENDIF

   RETURN cColor

