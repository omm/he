/* Copyright 2021 Rafał Jopek ( rafaljopek at hotmail com )
 *
 * he.prg
 *
 *  Simple program editor in Harbour.
 *
 *  Compile:    hbmk2 he.hbp
 *  Execute:    he <file>
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

   LOCAL lContinue := .T.
   LOCAL nLeft := 0, nMaxRow := 0, nMaxCol := 0
   LOCAL nKey, nKeyStd
   LOCAL aText
   LOCAL i, nLine
   LOCAL cString
   LOCAL cSubString
   LOCAL lToggleInsert := .F.

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
   hb_gtInfo( HB_GTI_WINTITLE, "Harbour Editor" )
   hb_gtInfo( HB_GTI_ICONFILE, "docs/assets/img/harbour.icns" )

   IF cFileName == NIL
      cFileName := "Untitled-1.prg"
   ENDIF

   DO WHILE lContinue

      aText := hb_ATokens( hb_MemoRead( "cFileName" ), .T. )

      IF nMaxRow != MaxRow() .OR. nMaxCol != MaxCol()
         nMaxRow := MaxRow()
         nMaxCol := MaxCol()
      ENDIF

      Scroll()
      DispBegin()

      hb_DispBox( 0, 0, nMaxRow, nMaxCol, "         ", 0x0 )

      FOR i := 0 TO nMaxRow

         nLine := i + nAddRow + 1

         IF nLine <= Len( aText )
            hb_DispOutAt( i, 0, PadR( aText[ nLine ], nMaxCol + 1 ), iif( i == nWRow, 0xf0, 0x7 ) )
         ELSE
            Scroll( i, 0, nMaxRow, nMaxCol )
            hb_DispOutAt( i, 0, ">> EOF <<", 0x01 )
            EXIT
         ENDIF

      NEXT

      DispEnd()

      SetPos( nWRow, nWCol )

      nKey := Inkey( 0 )

      nKeyStd := hb_keyStd( nKey )

      SWITCH nKeyStd

      CASE K_ESC                                    /*   Esc, Ctrl-[ */
         lContinue := .F.

         EXIT

      CASE K_LBUTTONDOWN                            /*   Mouse left button down */

         IF nWRow + 1 <= Len( aText ) .AND. MRow() + 1 <= Len( aText ) // dodano .AND. MRow() + 1 <= Len( aText )

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
               nAddRow -= nMaxRow + 1
            ENDIF
         ENDIF
         nWRow := 0

         EXIT

      CASE K_PGDN                                   /*   PgDn, Ctrl-C */

         IF nWRow >= nMaxRow
            IF nAddRow +  nMaxRow + 2 <= Len( aText )
               nAddRow += nMaxRow + 2
            ENDIF
         ENDIF

         nWRow := Min( nMaxRow, Len( aText ) - nAddRow - 1 )

         Scroll( 0, nLeft, nMaxRow, nMaxCol )

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

         IF lToggleInsert
            SetCursor( SC_NORMAL )
            lToggleInsert := .F.
         ELSE
            SetCursor( SC_INSERT )
            lToggleInsert := .T.
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
         aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lToggleInsert, 1, 0 ), "   " )

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

            aText[ nAddRow + nWRow + 1 ] := Stuff( cString, nWCol + 1, iif( lToggleInsert, 1, 0 ), AddChar( nKeyStd ) )

            nWCol++

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
