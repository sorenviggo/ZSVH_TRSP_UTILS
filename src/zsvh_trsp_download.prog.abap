*&---------------------------------------------------------------------*
*& Report ZSVH_TRSP_DOWNLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSVH_TRSP_DOWNLOAD.

  class lcx_exception DEFINITION INHERITING FROM CX_STATIC_CHECK.
    PUBLIC SECTION.
      methods:
        CONSTRUCTOR
          IMPORTING
            !TEXTID   like TEXTID   optional
            !PREVIOUS like PREVIOUS optional
            !MSG_TEXT type STRING   optional,
        IF_MESSAGE~GET_TEXT REDEFINITION .
    PRIVATE SECTION.
      data MSG_TEXT type STRING.
  endclass.
  class lcl_tr_util DEFINITION.
    PUBLIC SECTION.
      TYPES:
        begin of typ_s_trkorr,
          TRKORR  type TRKORR,
        end of typ_s_trkorr,
        typ_t_trkorr type hashed table of typ_s_trkorr WITH UNIQUE KEY TRKORR,
        begin of typ_s_alv_data,
          TRKORR      type TRKORR,
          TRFUNCTION  type TRFUNCTION,
          TRFUNC_DESC type VAL_TEXT,
          TRSTATUS    type TRSTATUS,
          TRSTAT_DESC type VAL_TEXT,
          AS4USER     type TR_AS4USER,
          AS4DATE     type AS4DATE,
          AS4TIME     type AS4TIME,
          AS4TEXT     type AS4TEXT,
          IS_TR_DB    type ABAP_BOOL,  " Does TR exist in DB
          IC_TR_DB    type ICON_D,
          IS_TR_COFL  type ABAP_BOOL,  " Does TR Cofile exist ?
          IC_TR_COFL  type ICON_D,
          IS_TR_DATA  type ABAP_BOOL,  " Does TR Data file exist ?
          IC_TR_DATA  type ICON_D,
          IS_TR_DWNLD type ABAP_BOOL,  " Will something be downloaded ?
          IC_TR_DWNLD type ICON_D,
          PATH_COFL   type LOCALFILE,
          FILE_COFL   type LOCALFILE,
          PATH_DATA   type LOCALFILE,
          FILE_DATA   type LOCALFILE,
          MESSAGE     type STRING,
        end of typ_s_alv_data,
        typ_t_alv_data type table of typ_s_alv_data WITH KEY TRKORR.
      class-METHODS:
*        CLASS_CONSTRUCTOR,
        GET_FUNC_DESC
          IMPORTING
            IV_FUNC type TRFUNCTION
          RETURNING
            VALUE(RV_FUNC_DESC) type VAL_TEXT,
        GET_STATUS_DESC
          IMPORTING
            IV_FUNC type TRSTATUS
          RETURNING
            VALUE(RV_STAT_DESC) type VAL_TEXT,
        GET_TRANS_DESC
          IMPORTING
            IV_TRKORR  type TRKORR
          RETURNING
            VALUE(RV_DESC) type AS4TEXT,
        GET_TRANS_LIST
          IMPORTING
            IT_TRANS_LIST  type typ_t_trkorr
          RETURNING
            VALUE(RT_ALV_DATA) type typ_t_alv_data,
        GET_DATA_FILE_NAME
          IMPORTING
            IV_TRKORR  type TRKORR
          RETURNING
            VALUE(RV_FILENAME) type LOCALFILE,
        GET_COFL_FILE_NAME
          IMPORTING
            IV_TRKORR  type TRKORR
          RETURNING
            VALUE(RV_FILENAME) type LOCALFILE,
        GET_SRV_TRANS_DATA_PATH
          IMPORTING
            IV_TRKORR  type TRKORR
          RETURNING
            VALUE(RV_PATH) type LOCALFILE
          RAISING
            LCX_EXCEPTION,
        GET_SRV_TRANS_COFILES_PATH
          IMPORTING
            IV_TRKORR  type TRKORR
          RETURNING
            VALUE(RV_PATH) type LOCALFILE
          RAISING
            LCX_EXCEPTION,
        DOES_SRV_FOLDER_EXIST
          IMPORTING
            IV_FOLDER_PATH  type LOCALFILE
          RETURNING
            VALUE(RV_FOLDER_EXISTS) type ABAP_BOOL,
        DOES_SRV_FILE_EXIST
          IMPORTING
            IV_FILE_PATH  type LOCALFILE
          RETURNING
            VALUE(RV_FILE_EXISTS) type ABAP_BOOL,
        CREATE_CLIENT_FOLDERS
          IMPORTING
            ROOT_FOLDER type STRING
          RAISING
            LCX_EXCEPTION,
        GET_BOOL_ICON
          IMPORTING
            IV_BOOL  type ABAP_BOOL
          RETURNING
            VALUE(RV_ICON)  type ICON_D,
        GUI_SELECT_FOLDER_F4
          RETURNING
            VALUE(RV_FOLDER) type STRING,
        DOWNLOAD_FILES
          IMPORTING
            IV_ROOT_PATH type STRING
          CHANGING
            CT_ALV_DATA  type typ_t_alv_data
          RAISING
            LCX_EXCEPTION,
        DISPLAY_ALV_DATA
          IMPORTING
            IT_ALV_DATA type typ_t_alv_data.
    PRIVATE SECTION.
      TYPES:
        begin of TYP_S_DOMV_KEYVAL,
          KEY   type VAL_SINGLE,
          VALUE type VAL_TEXT,
        end of TYP_S_DOMV_KEYVAL.
      TYPES:
        TYP_T_DOMV_KEYVAL TYPE HASHED TABLE OF TYP_S_DOMV_KEYVAL WITH UNIQUE KEY KEY.
      CLASS-DATA:
        GT_FUNC_DESC type TYP_T_DOMV_KEYVAL,
        GT_STAT_DESC type TYP_T_DOMV_KEYVAL,
        GV_SRV_PATH_DIR_TRANS type LOCALFILE,
        GV_SRV_PATH_DIR_DATA  type LOCALFILE,
        GV_SRV_PATH_DIR_COFL  type LOCALFILE,
        GV_CLI_PATH_DIR_PACK  type LOCALFILE,
        GV_CLI_PATH_DIR_DATA  type LOCALFILE,
        GV_CLI_PATH_DIR_COFL  type LOCALFILE.
      CLASS-METHODS:
        GET_SRV_PATH_DIR_TRANS
          RETURNING
            VALUE(RV_PATH) type LOCALFILE
          RAISING
            LCX_EXCEPTION.

  endclass.


  data:
  gx_root      type ref to CX_ROOT,
  gv_trkorr    type E070-TRKORR,
  gv_root_path type STRING,
  go_tr_util   type ref to lcl_tr_util,
  gs_trans     type lcl_tr_util=>typ_s_trkorr,
  gt_trans     type lcl_tr_util=>typ_t_trkorr,
  gs_alv_data  type lcl_tr_util=>typ_s_alv_data,
  gt_alv_data  type lcl_tr_util=>typ_t_alv_data.

* Transport selection.
  SELECTION-SCREEN BEGIN OF BLOCK B100 WITH FRAME TITLE title100.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(30) l_trkorr.
      SELECT-OPTIONS s_trkorr  for gv_trkorr OBLIGATORY no INTERVALS.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK B100.

* Download folder.
  SELECTION-SCREEN BEGIN OF BLOCK B500 WITH FRAME TITLE title500.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(33) l_path.
      PARAMETERS p_path type LOCALFILE.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK B500.


* Test.
  SELECTION-SCREEN BEGIN OF BLOCK B900 WITH FRAME TITLE title900.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 5(30) l_test.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK B900.



INITIALIZATION.

* Set name scheme for tansport.
  s_trkorr-LOW = sy-sysid && 'K'.
  insert table s_trkorr.

* Set fixed labels.
  title100  = 'Transport selections'.
  l_trkorr  = 'Transport Request'.
  title500  = 'Download File location'.
  l_path    = 'Target Folder path'.
  title900  = 'Test (no download of files)'.
  l_test    = 'Test'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.

  p_path = lcl_tr_util=>GUI_SELECT_FOLDER_F4( ).

START-OF-SELECTION.

* Check input.
  loop at s_trkorr where OPTION <> 'EQ' OR SIGN <> 'I'.
    message 'Only including single values are allowed for transport requests!' TYPE 'S' DISPLAY LIKE 'E'.
    exit.
  endloop.

* Convert data.
  loop at s_trkorr where OPTION = 'EQ' AND SIGN = 'I' AND LOW is not initial.
    clear gs_trans.
    gs_trans-TRKORR = s_trkorr-LOW.
    insert gs_trans into table gt_trans.
  endloop.

* Get ALV data.
  gt_alv_data = lcl_tr_util=>GET_TRANS_LIST( gt_trans ).

* Not test run ?
  if p_test = ABAP_FALSE.
*   Path must be provided when not test-run.
    if p_path is initial.
      message 'Target path must be provided, when not in test mode' TYPE 'S' DISPLAY LIKE 'E'.
      exit.
    endif.
*   Download files.
    TRY.
      gv_root_path = p_path.
      lcl_tr_util=>DOWNLOAD_FILES(
        EXPORTING
          IV_ROOT_PATH = gv_root_path
        CHANGING
          CT_ALV_DATA = gt_alv_data ).
    CATCH CX_ROOT into gx_root.
      message gx_root->GET_TEXT( ) TYPE 'S' DISPLAY LIKE 'E'.
      exit.
    ENDTRY.
  endif.

* Display ALV data.
  lcl_tr_util=>DISPLAY_ALV_DATA( gt_alv_data ).


***************************
* Helper Class defintion. *
***************************
class lcl_tr_util IMPLEMENTATION.
*  method CLASS_CONSTRUCTOR.
*    LOAD_DB_TEXTS( ).
*  endmethod.

  method GET_TRANS_LIST.
*   IMPORTING
*     IT_TRANS_LIST  type typ_t_trkorr
*   RETURNING
*     VALUE(RT_ALV_DATA) type typ_t_alv_data.
    data:
    lx_root      type ref to CX_ROOT,
    ls_trkorr    type typ_s_trkorr,
    ls_alv_data  type typ_s_alv_data,
    ls_E070      type E070.

*   Examine transports.
    loop at IT_TRANS_LIST into ls_trkorr.
*     Set Transport ID
      ls_alv_data-TRKORR = ls_trkorr-TRKORR.
*     Get header data of transport from DB.
      select single * into ls_E070
      from E070
      where TRKORR = ls_trkorr-TRKORR.
      if sy-subrc = 0.
*       Copy fields.
        ls_alv_data-TRFUNCTION = ls_E070-TRFUNCTION.
        ls_alv_data-TRSTATUS   = ls_E070-TRSTATUS.
        ls_alv_data-AS4USER    = ls_E070-AS4USER.
        ls_alv_data-AS4DATE    = ls_E070-AS4DATE.
        ls_alv_data-AS4TIME    = ls_E070-AS4TIME.
        ls_alv_data-AS4TEXT    = GET_TRANS_DESC( ls_alv_data-TRKORR ).
        MOVE-CORRESPONDING ls_E070 to ls_alv_data.
*       Get function and status description.
        ls_alv_data-TRFUNC_DESC = GET_FUNC_DESC( ls_alv_data-TRFUNCTION ).
        ls_alv_data-TRSTAT_DESC = GET_STATUS_DESC( ls_alv_data-TRSTATUS ).
*       Mark that entry was found in DB.
        ls_alv_data-IS_TR_DB = ABAP_TRUE.
      endif.
*     Get paths to data files and check existence
      TRY.
        ls_alv_data-PATH_DATA  = GET_SRV_TRANS_DATA_PATH( ls_alv_data-TRKORR ).
        ls_alv_data-FILE_DATA  = GET_DATA_FILE_NAME( ls_alv_data-TRKORR ).
        ls_alv_data-IS_TR_DATA = DOES_SRV_FILE_EXIST( ls_alv_data-PATH_DATA ).
      CATCH CX_ROOT into lx_root.
        ls_alv_data-MESSAGE    = lx_root->GET_TEXT( ).
        ls_alv_data-IS_TR_DATA = ABAP_FALSE. " Not necessary - but just for clarity.
      ENDTRY.
*     Get paths to cofiles and check existence
      TRY.
        ls_alv_data-PATH_COFL  = GET_SRV_TRANS_COFILES_PATH( ls_alv_data-TRKORR ).
        ls_alv_data-FILE_COFL  = GET_COFL_FILE_NAME( ls_alv_data-TRKORR ).
        ls_alv_data-IS_TR_COFL = DOES_SRV_FILE_EXIST( ls_alv_data-PATH_COFL ).
      CATCH CX_ROOT into lx_root.
        ls_alv_data-MESSAGE    = lx_root->GET_TEXT( ).
        ls_alv_data-IS_TR_COFL = ABAP_FALSE. " Not necessary - but just for clarity.
      ENDTRY.
*     Set overall status.
      if ls_alv_data-IS_TR_DATA = ABAP_TRUE AND ls_alv_data-IS_TR_COFL = ABAP_TRUE.
        ls_alv_data-IS_TR_DWNLD = ABAP_TRUE.
        ls_alv_data-MESSAGE     = 'All files exists'.
      else.
        ls_alv_data-IS_TR_DWNLD = ABAP_FALSE.
        ls_alv_data-MESSAGE     = 'At least one file did not exist - none will be downloaded'.
      endif.
*     Set Icons.
      ls_alv_data-IC_TR_DB    = GET_BOOL_ICON( ls_alv_data-IS_TR_DB ).
      ls_alv_data-IC_TR_DATA  = GET_BOOL_ICON( ls_alv_data-IS_TR_DATA ).
      ls_alv_data-IC_TR_COFL  = GET_BOOL_ICON( ls_alv_data-IS_TR_COFL ).
      ls_alv_data-IC_TR_DWNLD = GET_BOOL_ICON( ls_alv_data-IS_TR_DWNLD ).
*     Add to result.
      append ls_alv_data to RT_ALV_DATA.
    endloop.



  endmethod.

  method GET_FUNC_DESC.
*   IMPORTING
*     IV_FUNC type TRFUNCTION
*   RETURNING
*     VALUE(RV_FUNC_DESC) type VAL_TEXT,
    data:
    ls_func_desc  type TYP_S_DOMV_KEYVAL.
    READ TABLE GT_FUNC_DESC into ls_func_desc
    WITH TABLE KEY KEY = IV_FUNC.
    if sy-subrc = 0.
*     Matched => Return result.
      RV_FUNC_DESC = ls_func_desc-VALUE.
    else.
*     Not matched => Get from DB.
      ls_func_desc-KEY = IV_FUNC.
      select single DDTEXT into ls_func_desc-VALUE
      from DD07T
      where DOMNAME    = 'TRFUNCTION'
        AND DDLANGUAGE = 'E'
        AND AS4LOCAL   = 'A'
        AND DOMVALUE_L = ls_func_desc-KEY.
*     Update Cache.
      insert ls_func_desc into table GT_FUNC_DESC.
*     Return result.
      RV_FUNC_DESC = ls_func_desc-VALUE.
    endif.
  endmethod.

  method GET_STATUS_DESC.
*   IMPORTING
*     IV_FUNC type TRSTATUS
*   RETURNING
*     VALUE(RV_STAT_DESC) type VAL_TEXT,
    data:
    ls_stat_desc  type TYP_S_DOMV_KEYVAL.
    READ TABLE GT_STAT_DESC into ls_stat_desc
    WITH TABLE KEY KEY = IV_FUNC.
    if sy-subrc = 0.
*     Matched => Return result.
      RV_STAT_DESC = ls_stat_desc-VALUE.
    else.
*     Not matched => Get from DB.
      ls_stat_desc-KEY = IV_FUNC.
      select single DDTEXT into ls_stat_desc-VALUE
      from DD07T
      where DOMNAME    = 'TRSTATUS'
        AND DDLANGUAGE = 'E'
        AND AS4LOCAL   = 'A'
        AND DOMVALUE_L = ls_stat_desc-KEY.
*     Update Cache.
      insert ls_stat_desc into table GT_STAT_DESC.
*     Return result.
      RV_STAT_DESC = ls_stat_desc-VALUE.
    endif.
  endmethod.

  method GET_TRANS_DESC.
*   IMPORTING
*     IV_TRKORR  type TRKORR
*   RETURNING
*     VALUE(RV_DESC) type AS4TEXT,

*   Attempt to get description in logon language.
    select single AS4TEXT into RV_DESC
    from E07T
    where TRKORR = IV_TRKORR
      AND LANGU  = sy-langu.
    if RV_DESC is initial.
*     No luck => Get description in any language.
      select single AS4TEXT into RV_DESC
      from E07T
      where TRKORR = IV_TRKORR
        AND AS4TEXT <> ''.
    endif.

  endmethod.

  method GET_DATA_FILE_NAME.
*   IMPORTING
*     IV_TRKORR  type TRKORR
*   RETURNING
*     VALUE(RV_FILENAME) type LOCALFILE,

*   Construct filename.
    RV_FILENAME = 'R' && IV_TRKORR+4(6) && '.' && IV_TRKORR(3).

  endmethod.

  method GET_COFL_FILE_NAME.
*   IMPORTING
*     IV_TRKORR  type TRKORR
*   RETURNING
*     VALUE(RV_FILENAME) type LOCALFILE,

*   Construct filename.
    RV_FILENAME = 'K' && IV_TRKORR+4(6) && '.' && IV_TRKORR(3).

  endmethod.

  method GET_SRV_TRANS_DATA_PATH.
*   IMPORTING
*     IV_TRKORR  type TRKORR
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE
*   RAISING
*     LCX_EXCEPTION

    data:
    lv_full      type string,
    lv_path_root type CHAR128,
    lv_path_data TYPE CHAR128,
    lv_path_file TYPE CHAR128,
    lv_filename  type CHAR128.

*   Transport ID must be of form SSSK9NNNNN, where SSS is system ID and NNNNN is a sequential numbering.
    if strlen( IV_TRKORR ) <> 10.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Data file: Transport ID must be exactly 10 chars long in order for this logic to work.'.
    endif.

    if GV_SRV_PATH_DIR_DATA is not initial.
*     Read from cache.
      lv_path_data = GV_SRV_PATH_DIR_DATA.
    else.
*     Get root path to DIR_TRANS.
      lv_path_root = GET_SRV_PATH_DIR_TRANS( ).
*     Construct path to data folder.
      CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD 'data'
                           ID 'PATH'     FIELD lv_path_root
                           ID 'RESULT'   FIELD lv_path_data.
      if sy-subrc <> 0.
        RAISE EXCEPTION TYPE LCX_EXCEPTION
        EXPORTING
          MSG_TEXT = 'Path to data folder could not be established'.
      endif.
*     Update cache.
      GV_SRV_PATH_DIR_DATA = lv_path_data.
    endif.

*   Construct filename.
    lv_filename = GET_DATA_FILE_NAME( IV_TRKORR ).

*   Construct path to transport data file.
    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_filename
                         ID 'PATH'     FIELD lv_path_data
                         ID 'RESULT'   FIELD lv_path_file.
    if sy-subrc <> 0.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Path to transport data file could not be established'.
    endif.

*   Set result.
    RV_PATH = lv_path_file.

  endmethod.

  method GET_SRV_TRANS_COFILES_PATH.
*   IMPORTING
*     IV_TRKORR  type TRKORR
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE,
*   RAISING
*     LCX_EXCEPTION,

    data:
    lv_full      type string,
    lv_path_root type CHAR128,
    lv_path_cofl TYPE CHAR128,
    lv_path_file TYPE CHAR128,
    lv_filename  type CHAR128.

*   Transport ID must be of form SSSK9NNNNN, where SSS is system ID and NNNNN is a sequential numbering.
    if strlen( IV_TRKORR ) <> 10.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Cofiles: Transport ID must be exactly 10 chars long in order for this logic to work.'.
    endif.

    if GV_SRV_PATH_DIR_COFL is not initial.
*     Read from cache.
      lv_path_cofl = GV_SRV_PATH_DIR_COFL.
    else.
*     Get root path to DIR_TRANS.
      lv_path_root = GET_SRV_PATH_DIR_TRANS( ).
*     Construct path to data folder.
      CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD 'cofiles'
                           ID 'PATH'     FIELD lv_path_root
                           ID 'RESULT'   FIELD lv_path_cofl.
      if sy-subrc <> 0.
        RAISE EXCEPTION TYPE LCX_EXCEPTION
        EXPORTING
          MSG_TEXT = 'Path to cofiles folder could not be established'.
      endif.
*     Update cache.
      GV_SRV_PATH_DIR_COFL = lv_path_cofl.
    endif.

*   Construct filename.
    lv_filename = GET_COFL_FILE_NAME( IV_TRKORR )..

*   Construct path to transport data file.
    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_filename
                         ID 'PATH'     FIELD lv_path_cofl
                         ID 'RESULT'   FIELD lv_path_file.
    if sy-subrc <> 0.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Path to transport cofiles file could not be established'.
    endif.

*   Set result.
    RV_PATH = lv_path_file.

  endmethod.

  method CREATE_CLIENT_FOLDERS.
*   IMPORTING
*     ROOT_FOLDER type STRING
*   RAISING
*     LCX_EXCEPTION

    data:
    lv_path_root    type STRING,
    lv_dir_pack     type STRING,
    lv_path_pack    type STRING,
    lv_dir_data     type STRING,
    lv_path_data    type STRING,
    lv_dir_cofl     type STRING,
    lv_path_cofl    type STRING,
    lv_file_sep     type C,
    lv_root_exists  type ABAP_BOOL,
    lv_root_len     type I,
    lv_rc           type I,
    lv_msg          type STRING.

*   Get path separator of client OS.
    CL_GUI_FRONTEND_SERVICES=>get_file_separator( CHANGING FILE_SEPARATOR = lv_file_sep ).

*   Set root path
    lv_root_len = strlen( ROOT_FOLDER ) - 1.
    if ROOT_FOLDER+lv_root_len = lv_file_sep.
      lv_path_root = ROOT_FOLDER(lv_root_len).
    else.
      lv_path_root = ROOT_FOLDER.
    endif.

*   Root path must not be initial.
    if lv_path_root is initial.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = 'Root folder path is empty!'.
    endif.

*   This cannot be done in background
    if sy-batch is not initial.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = 'Folders cannot be created in background - only via GUI'.
    endif.

*   Check that root folder exists.
    lv_root_exists = CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST( lv_path_root ).
    if lv_root_exists <> ABAP_TRUE.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = |Root folder does not exist on client - { lv_path_root }|.
    endif.


*   Construct package folder name.
    GET TIME.
    lv_dir_pack = 'Transports_' && sy-sysid && '_' && sy-datum && '_' && sy-uzeit.
    lv_path_pack = lv_path_root && lv_file_sep && lv_dir_pack.

*   Create package folder.
    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE(
      EXPORTING
        DIRECTORY = lv_path_pack
      CHANGING
        RC = lv_rc
      EXCEPTIONS
        OTHERS = 99 ).
    if sy-subrc <> 0.
      if sy-MSGID is not initial AND sy-MSGNO is not initial.
        message id sy-msgid type 'S' number sy-msgno into lv_msg with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        lv_msg = `Error while creating package folder on client - ` && lv_msg.
      else.
        lv_msg = 'Package folder could not be created on client'.
      endif.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = lv_msg.
    endif.

*   Save package path in static variable.
    GV_CLI_PATH_DIR_PACK = lv_path_pack.

*   Construct path to data folder in package
    lv_path_data = lv_path_pack && lv_file_sep && 'data'.

*   Create package folder.
    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE(
      EXPORTING
        DIRECTORY = lv_path_data
      CHANGING
        RC = lv_rc
      EXCEPTIONS
        OTHERS = 99 ).
    if sy-subrc <> 0.
      if sy-MSGID is not initial AND sy-MSGNO is not initial.
        message id sy-msgid type 'S' number sy-msgno into lv_msg with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        lv_msg = `Error while creating data folder on client - ` && lv_msg.
      else.
        lv_msg = 'Data folder could not be created on client'.
      endif.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = lv_msg.
    endif.

*   Save data path in static variable.
    GV_CLI_PATH_DIR_DATA = lv_path_data.


*   Construct path to cofiles folder in package
    lv_path_cofl = lv_path_pack && lv_file_sep && 'cofiles'.

*   Create package folder.
    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_CREATE(
      EXPORTING
        DIRECTORY = lv_path_cofl
      CHANGING
        RC = lv_rc
      EXCEPTIONS
        OTHERS = 99 ).
    if sy-subrc <> 0.
      if sy-MSGID is not initial AND sy-MSGNO is not initial.
        message id sy-msgid type 'S' number sy-msgno into lv_msg with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        lv_msg = `Error while creating cofiles folder on client - ` && lv_msg.
      else.
        lv_msg = 'Cofiles folder could not be created on client'.
      endif.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = lv_msg.
    endif.

*   Save cofiles path in static variable.
    GV_CLI_PATH_DIR_COFL = lv_path_cofl.

*   This far - folders were created.
    message |Package folder { lv_dir_pack } was created.| type 'S'.

  endmethod.

  method GET_BOOL_ICON.
*   IMPORTING
*     IV_BOOL  type ABAP_BOOL
*   RETURNING
*     VALUE(RV_ICON)  type ICON_D,
    if IV_BOOL = ABAP_TRUE.
      RV_ICON = ICON_LED_GREEN.
    else.
      RV_ICON = ICON_LED_RED.
    endif.
  endmethod.

  method GUI_SELECT_FOLDER_F4.
*   RETURNING
*     VALUE(RV_FOLDER) type STRING,

    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE(
      EXPORTING
        WINDOW_TITLE    = 'Select download folder'
        INITIAL_FOLDER  = ''
      CHANGING
        SELECTED_FOLDER = RV_FOLDER ).


  endmethod.

  method DOWNLOAD_FILES.
*   IMPORTING
*     IV_ROOT_PATH type STRING
*   CHANGING
*     CT_ALV_DATA  type typ_t_alv_data
*   RAISING
*     LCX_EXCEPTION,

    data:
    lv_path_data    type STRING,
    lv_path_cofl    type STRING,
    lv_srv_data     type SAEPFAD,
    lv_cli_data     type SAEPFAD,
    lv_srv_cofl     type SAEPFAD,
    lv_cli_cofl     type SAEPFAD,
    lv_file_sep     type C.

    field-symbols:
    <fs_alv_data>  type typ_s_alv_data.

*   Get path separator of client OS.
    CL_GUI_FRONTEND_SERVICES=>get_file_separator( CHANGING FILE_SEPARATOR = lv_file_sep ).

*   Check that something is ready for download.
    loop at CT_ALV_DATA assigning <fs_alv_data> where IS_TR_DWNLD = ABAP_TRUE.
      exit.
    endloop.
    if sy-subrc <> 0.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = 'No files ready for download!'.
    endif.

*   Create client folders.
    CREATE_CLIENT_FOLDERS( IV_ROOT_PATH ).

*   Download files.
    loop at CT_ALV_DATA assigning <fs_alv_data> where IS_TR_DWNLD = ABAP_TRUE.

*     Construct path to client files.
      lv_path_data = GV_CLI_PATH_DIR_DATA && lv_file_sep && <fs_alv_data>-FILE_DATA.
      lv_path_cofl = GV_CLI_PATH_DIR_COFL && lv_file_sep && <fs_alv_data>-FILE_COFL.

*     Check length of paths (FM ARCHIVFILE_SERVER_TO_CLIENT has limit of 70 chars due to data element SAEPFAD)
      if strlen( <fs_alv_data>-PATH_DATA ) > 70.
        <fs_alv_data>-MESSAGE    = 'Path to server Data file is longer than 70 chars.'.
        continue.
      endif.
      if strlen( lv_path_data ) > 70.
        <fs_alv_data>-MESSAGE    = 'Path to client Data file is longer than 70 chars.'.
        continue.
      endif.
      if strlen( <fs_alv_data>-PATH_COFL ) > 70.
        <fs_alv_data>-MESSAGE    = 'Path to server Cofiles file is longer than 70 chars.'.
        continue.
      endif.
      if strlen( lv_path_cofl ) > 70.
        <fs_alv_data>-MESSAGE    = 'Path to client Cofiles file is longer than 70 chars.'.
        continue.
      endif.

*     Do type conversions of paths.
      lv_srv_data = <fs_alv_data>-PATH_DATA.
      lv_cli_data = lv_path_data.
      lv_srv_cofl = <fs_alv_data>-PATH_COFL.
      lv_cli_cofl = lv_path_cofl.


*     Download data file.
      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
        EXPORTING
          PATH                   = lv_srv_data
          TARGETPATH             = lv_cli_data
        EXCEPTIONS
          OTHERS                 = 99.
      IF SY-SUBRC <> 0.
        <fs_alv_data>-IS_TR_DATA = ABAP_FALSE.
        <fs_alv_data>-MESSAGE    = 'Data file - Download error - Cofile is not downloaded'.
        continue.
      ENDIF.

*     Download cofiles file.
      CALL FUNCTION 'ARCHIVFILE_SERVER_TO_CLIENT'
        EXPORTING
          PATH                   = lv_srv_cofl
          TARGETPATH             = lv_cli_cofl
        EXCEPTIONS
          OTHERS                 = 99.
      IF SY-SUBRC <> 0.
        <fs_alv_data>-IS_TR_COFL = ABAP_FALSE.
        <fs_alv_data>-MESSAGE    = 'Cofiles file - Download error'.
      ELSE.
        <fs_alv_data>-MESSAGE    = 'Files downloaded'.
      ENDIF.

*     Set Icons
      <fs_alv_data>-IC_TR_DATA = GET_BOOL_ICON( <fs_alv_data>-IS_TR_DATA ).
      <fs_alv_data>-IC_TR_COFL = GET_BOOL_ICON( <fs_alv_data>-IS_TR_COFL ).
    endloop.


  endmethod.

  method DISPLAY_ALV_DATA.
*   IMPORTING
*     IT_ALV_DATA type typ_t_alv_data.

    DATA:
    lt_alv_data   type typ_t_alv_data,
    lx_salv_error TYPE REF TO cx_salv_error,
    lo_alv        TYPE REF TO cl_salv_table,
    lo_functions  TYPE REF TO cl_salv_functions_list,
    lo_display    type ref to cl_salv_display_settings,
    lo_layout     type ref to cl_salv_layout,
    lo_cols       TYPE REF TO cl_salv_columns_table,
    lo_col        TYPE REF TO cl_salv_column_table,
    ls_layout_key type SALV_S_LAYOUT_KEY,
    ls_columns    TYPE salv_s_column_ref,
    lt_columns    TYPE salv_t_column_ref.

*   Copy data.
    lt_alv_data = IT_ALV_DATA.

    TRY.
*     Get ALV instance.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = lt_alv_data ).

*     Optimize columns.
      lo_cols = lo_alv->get_columns( ).
      lo_cols->set_optimize( 'X' ).

*     Modify columns.
      lt_columns = lo_cols->get( ).
      loop at lt_columns into ls_columns.
        lo_col ?= ls_columns-R_COLUMN.
        case ls_columns-COLUMNNAME.
          when 'IS_TR_DB' OR 'IS_TR_COFL' OR 'IS_TR_DATA' OR 'IS_TR_DWNLD' OR 'FILE_COFL' OR 'FILE_DATA'.
            ls_columns-R_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          when 'IC_TR_DB'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'In DB' ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'IC_TR_DATA'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Data file' ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'IC_TR_COFL'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Cofiles' ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'IC_TR_DWNLD'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Status' ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'PATH_COFL'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Cofiles path' ).
          when 'PATH_DATA'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Data file path' ).
          when 'MESSAGE'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Message' ).

        endcase.
      endloop.

*     when 'PERNR'.
*       lo_col ?= ls_columns-R_COLUMN.
*       lo_col->SET_CELL_TYPE( if_salv_c_cell_type=>hotspot ).
*       ls_columns-R_COLUMN->SET_MEDIUM_TEXT( text-930 ).

*     Set functions.
      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( 'X' ).

*     Set title.
      lo_display = lo_alv->get_display_settings( ).
      lo_display->set_list_header( 'Transport statuses' ).

*     Display table.
      lo_alv->display( ).
    CATCH cx_salv_error INTO lx_salv_error.
      message 'ALV error: ' && lx_salv_error->GET_TEXT( ) type 'S' display like 'E'.
      exit.
    ENDTRY.


  endmethod.

  method DOES_SRV_FOLDER_EXIST.
*   IMPORTING
*     IV_FOLDER_PATH  type LOCALFILE
*   RETURNING
*     VALUE(RV_FOLDER_EXISTS) type ABAP_BOOL,

    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
         DIRECTORY_LONG                   = IV_FOLDER_PATH
      EXCEPTIONS
        PFL_DIR_NOT_EXIST                 = 1
        PFL_PERMISSION_DENIED             = 2
        PFL_CANT_BUILD_DATASET_NAME       = 3
        PFL_FILE_NOT_EXIST                = 4
        PFL_AUTHORIZATION_MISSING         = 5
        OTHERS                            = 6.
    IF SY-SUBRC = 0.
      RV_FOLDER_EXISTS = ABAP_TRUE.
    ELSE.
      RV_FOLDER_EXISTS = ABAP_FALSE.
    ENDIF.

  endmethod.

  method DOES_SRV_FILE_EXIST.
*   IMPORTING
*     IV_FILE_PATH  type LOCALFILE
*   RETURNING
*     VALUE(RV_FILE_EXISTS) type ABAP_BOOL,

    open dataset IV_FILE_PATH for input
         in text mode encoding default
         ignoring conversion errors.
    IF SY-SUBRC = 0.
      RV_FILE_EXISTS = ABAP_TRUE.
    ELSE.
      RV_FILE_EXISTS = ABAP_FALSE.
    ENDIF.
    close dataset IV_FILE_PATH.


  endmethod.

*******************
* Private methods.
*******************
  method GET_SRV_PATH_DIR_TRANS.
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE
*   EXCEPTIONS
*     LCX_EXCEPTION.

    data:
    lv_path_trans  TYPE C LENGTH 128.

*   Check if path has already been found.
    if GV_SRV_PATH_DIR_TRANS is not initial.
      RV_PATH = GV_SRV_PATH_DIR_TRANS.
      return.
    endif.

*   Establish path to DIR_TRANS.
    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TRANS'
                       ID 'VALUE' FIELD lv_path_trans.
    if lv_path_trans is not initial.
      GV_SRV_PATH_DIR_TRANS = lv_path_trans.
      RV_PATH               = GV_SRV_PATH_DIR_TRANS.
    else.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Path not found for directory DIR_TRANS'.
    endif.


  endmethod.

endclass.

class lcx_exception IMPLEMENTATION.
  method CONSTRUCTOR.
*   IMPORTING
*     !TEXTID   like TEXTID   optional
*     !PREVIOUS like PREVIOUS optional
*     !MSG_TEXT type STRING   optional,
    CALL METHOD SUPER->CONSTRUCTOR
    EXPORTING
      TEXTID = TEXTID
      PREVIOUS = PREVIOUS.

    me->MSG_TEXT = MSG_TEXT .
  endmethod.

  method IF_MESSAGE~GET_TEXT.
    if me->MSG_TEXT is not initial.
      RESULT = me->MSG_TEXT.
    else.
      RESULT = super->GET_TEXT( ).
    endif.
  endmethod.
endclass.
