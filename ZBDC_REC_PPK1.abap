REPORT ZBDC_REC_PPK1
       NO STANDARD PAGE HEADING LINE-SIZE 255.

* INCLUDE BDCRECX1_S:
* THE CALL TRANSACTION USING IS CALLED WITH AUTHORITY-CHECK!
* IF YOU HAVE OWN AUTH.-CHECKS YOU CAN USE INCLUDE BDCRECX1 INSTEAD.
*INCLUDE BDCRECX1_S.

*START-OF-SELECTION.

*PERFORM OPEN_GROUP.

TYPES: BEGIN OF TY_MM,
         MATNR TYPE MATNR,
         MTART TYPE MTART,
         MBRSH TYPE MBRSH,
         MEINS TYPE MEINS,
       END OF TY_MM.

DATA: ITAB TYPE TABLE OF TY_MM,
      WA   TYPE TY_MM.

DATA: ITAB_BDCDATA TYPE TABLE OF BDCDATA,
      WA_BDCDATA   TYPE BDCDATA.

DATA: ITAB_MSG TYPE TABLE OF BDCMSGCOLL.

PARAMETERS: IMP_FL TYPE LOCALFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR IMP_FL.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG       " MODULE POOL PROGRAM NAME FOR SCREEN FIELD
      DYNPRO_NUMBER = SYST-DYNNR       " DYNPRO NUMBER WHERE F4 HELP IS NEEDED
      FIELD_NAME    = SPACE            " NAME OF FIELD WHERE PATH IS TO BE ENTERED
    IMPORTING
      FILE_NAME     = IMP_FL.                " PATH NAME SELECTED BY USER WITH HELP OF FILEMNGR

START-OF-SELECTION.

  DATA: FL_STR TYPE STRING.
  FL_STR = IMP_FL.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = FL_STR                " NAME OF FILE
*     FILETYPE                = 'ASC'            " FILE TYPE (ASC OR BIN)
      HAS_FIELD_SEPARATOR     = 'X' "'SPACE'            " COLUMNS SEPARATED BY TABS IN CASE OF ASCII UPLOAD
*     HEADER_LENGTH           = 0                " LENGTH OF HEADER FOR BINARY DATA
*     READ_BY_LINE            = 'X'              " THE FILE WILL BE WRITTEN TO THE INTERNAL TABLE LINE-BY-LINE
*     DAT_MODE                = SPACE            " NUMERIC AND DATE FIELDS IMPORTED IN WS_DOWNLOAD 'DAT' FORMAT
*     CODEPAGE                =                  " CHARACTER REPRESENTATION FOR OUTPUT
*     IGNORE_CERR             = ABAP_TRUE        " SPECIFIES WHETHER TO IGNORE ERRORS CONVERTING CHARACTER SETS
*     REPLACEMENT             = '#'              " REPLACEMENT CHARACTER FOR NON-CONVERTIBLE CHARACTERS
*     CHECK_BOM               = SPACE            " THE CONSISTENCY OF THE CODEPAGE AND BYTE ORDER MARK WILL BE CHECKED
*     VIRUS_SCAN_PROFILE      =                  " VIRUS SCAN PROFILE
*     NO_AUTH_CHECK           = SPACE            " SWITCH OFF CHECK FOR ACCESS RIGHTS
*  IMPORTING
*     FILELENGTH              =                  " FILE LENGTH
*     HEADER                  =                  " FILE HEADER IN CASE OF BINARY UPLOAD
    TABLES
      DATA_TAB                = ITAB               " TRANSFER TABLE FOR FILE CONTENTS
*  CHANGING
*     ISSCANPERFORMED         = SPACE            " FILE ALREADY SCANNED
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1                " FILE DOES NOT EXIST AND CANNOT BE OPENED
      FILE_READ_ERROR         = 2                " ERROR WHEN READING FILE
      NO_BATCH                = 3                " FRONT-END FUNCTION CANNOT BE EXECUTED IN BACKGRND
      GUI_REFUSE_FILETRANSFER = 4                " INCORRECT FRONT END OR ERROR ON FRONT END
      INVALID_TYPE            = 5                " INCORRECT PARAMETER FILETYPE
      NO_AUTHORITY            = 6                " NO AUTHORIZATION FOR UPLOAD
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8                " CANNOT INTERPRET DATA IN FILE
      HEADER_NOT_ALLOWED      = 9                " INVALID HEADER
      SEPARATOR_NOT_ALLOWED   = 10               " INVALID SEPARATOR
      HEADER_TOO_LONG         = 11               " THE HEADER INFORMATION IS LIMITED TO 1023 BYTES AT PRESENT
      UNKNOWN_DP_ERROR        = 12               " ERROR WHEN CALLING DATA PROVIDER
      ACCESS_DENIED           = 13               " ACCESS TO FILE DENIED
      DP_OUT_OF_MEMORY        = 14               " NOT ENOUGH MEMORY IN DATA PROVIDER
      DISK_FULL               = 15               " STORAGE MEDIUM FULL
      DP_TIMEOUT              = 16               " TIMEOUT OF DATA PROVIDER
      OTHERS                  = 17.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ITAB INTO WA.

    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '0060'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RMMG1-MTART'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  'ENTR'.
    PERFORM BDC_FIELD       USING 'RMMG1-MATNR'
                                  WA-MATNR.
    PERFORM BDC_FIELD       USING 'RMMG1-MBRSH'
                                  WA-MBRSH.
    PERFORM BDC_FIELD       USING 'RMMG1-MTART'
                                  WA-MTART.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '0070'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MSICHTAUSW-DYTXT(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'MSICHTAUSW-KZSEL(01)'
                                  'X'.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '4004'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'MAKT-MAKTX'
                                  'BDC RECORDING PPK MM01'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MARA-MEINS'.
    PERFORM BDC_FIELD       USING 'MARA-MEINS'
                                  WA-MEINS.
    PERFORM BDC_FIELD       USING 'MARA-MSTAE'
                                  '02'.
    PERFORM BDC_FIELD       USING 'MARA-MSTDE'
                                  '06.08.2025'.
    PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0300'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=CANC'.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '4004'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM BDC_FIELD       USING 'MAKT-MAKTX'
                                  'BDC RECORDING PPK MM01'.
    PERFORM BDC_FIELD       USING 'MARA-MEINS'
                                  'KG'.
    PERFORM BDC_FIELD       USING 'MARA-MSTAE'
                                  '02'.
    PERFORM BDC_FIELD       USING 'MARA-MSTDE'
                                  '06.08.2025'.
*    PERFORM BDC_TRANSACTION USING 'MM01'.

*PERFORM CLOSE_GROUP.

    CALL TRANSACTION 'MM01' USING ITAB_BDCDATA MODE 'A' UPDATE 'S' MESSAGES INTO ITAB.
    CLEAR ITAB_BDCDATA.

  ENDLOOP.

*----------------------------------------------------------------------*
*        START NEW SCREEN                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM  = PROGRAM.
  WA_BDCDATA-DYNPRO   = DYNPRO.
  WA_BDCDATA-DYNBEGIN = 'X'.
  APPEND WA_BDCDATA TO ITAB_BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        INSERT FIELD                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> NODATA.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-FNAM = FNAM.
  WA_BDCDATA-FVAL = FVAL.
  APPEND WA_BDCDATA TO ITAB_BDCDATA.
*  ENDIF.
ENDFORM.
