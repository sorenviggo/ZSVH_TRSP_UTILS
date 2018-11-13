*&---------------------------------------------------------------------*
*& Report ZSVH_TRSP_UPLOAD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSVH_TRSP_UPLOAD.

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
  class lcl_tr_upload DEFINITION.
    PUBLIC SECTION.
      TYPES:
        begin of typ_s_alv_data,
          FILE_NAME  type LOCALFILE,
          TYPE       type STRING,
          PATH_CLI   type LOCALFILE,
          PATH_SRV   type LOCALFILE,
          SRV_FL_EX  type ABAP_BOOL,
          RC_STATUS  type I,
          IC_STATUS  type ICON_D,
          MESSAGE    type STRING,
        end of typ_s_alv_data,
        typ_t_alv_data type table of typ_s_alv_data WITH KEY FILE_NAME TYPE.
      CLASS-METHODS:
        GET_CLIENT_FILES
          IMPORTING
            IV_FOLDER_PATH type LOCALFILE
          RETURNING
            VALUE(RT_ALV_DATA) type typ_t_alv_data
          RAISING
            LCX_EXCEPTION,
        ADD_TARGET_INFO
          CHANGING
            CT_ALV_DATA type typ_t_alv_data
          RAISING
            LCX_EXCEPTION,
        GET_SRV_TRANS_FILE_PATH
          IMPORTING
            IV_FILENAME  type LOCALFILE
            IV_TYPE      type STRING
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
        UPLOAD_FILES
          CHANGING
            CT_ALV_DATA type typ_t_alv_data,
        GET_STATUS_ICON
          IMPORTING
            IV_STATUS  type I
          RETURNING
            VALUE(RV_ICON)  type ICON_D,
        GUI_SELECT_FOLDER_F4
          RETURNING
            VALUE(RV_FOLDER) type LOCALFILE,
        DISPLAY_ALV_DATA
          IMPORTING
            IT_ALV_DATA type typ_t_alv_data.

    PRIVATE SECTION.
      CONSTANTS:
        CON_FILE_TYPE_DATA  type C length 20 value 'DATA',
        CON_FILE_TYPE_COFL  type C length 20 value 'COFILES'.
      CLASS-DATA:
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
            LCX_EXCEPTION,
        GET_SRV_PATH_DIR_DATA
          RETURNING
            VALUE(RV_PATH) type LOCALFILE
          RAISING
            LCX_EXCEPTION,
        GET_SRV_PATH_DIR_COFL
          RETURNING
            VALUE(RV_PATH) type LOCALFILE
          RAISING
            LCX_EXCEPTION.


  endclass.

  data:
  gx_root      type ref to CX_ROOT,
  go_tr_upload type ref to lcl_tr_upload,
  gv_root_path type STRING,
  gs_alv_data  type lcl_tr_upload=>typ_s_alv_data,
  gt_alv_data  type lcl_tr_upload=>typ_t_alv_data.

* Upload (source) folder.
  SELECTION-SCREEN BEGIN OF BLOCK B500 WITH FRAME TITLE title500.
    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 1(33) l_path.
      PARAMETERS p_path type LOCALFILE OBLIGATORY.
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

* Set fixed labels.
  title500  = 'Source files'.
  l_path    = 'Source Folder path'.
  title900  = 'Test (no upload of files)'.
  l_test    = 'Test'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.

  p_path = lcl_tr_upload=>GUI_SELECT_FOLDER_F4( ).

START-OF-SELECTION.

  TRY.
*   Get client files.
    gt_alv_data = lcl_tr_upload=>GET_CLIENT_FILES( p_path ).
*   Add target info.
    lcl_tr_upload=>ADD_TARGET_INFO( CHANGING CT_ALV_DATA = gt_alv_data ).

*   Non-test mode => Upload files.
    if p_test = ABAP_FALSE.
      lcl_tr_upload=>UPLOAD_FILES( CHANGING CT_ALV_DATA = gt_alv_data ).
    endif.

  CATCH CX_ROOT into gx_root.
    message gx_root->GET_TEXT( ) type 'S' DISPLAY LIKE 'E'.
    exit.
  ENDTRY.

* Display ALV data.
  lcl_tr_upload=>DISPLAY_ALV_DATA( gt_alv_data ).


class lcl_tr_upload IMPLEMENTATION.
  method GET_CLIENT_FILES.
*   IMPORTING
*     IV_FOLDER_PATH type LOCALFILE
*   RETURNING
*     VALUE(RT_ALV_DATA) type typ_t_alv_data,
    data:
    lv_file_sep     type C,
    lv_path_data    type STRING,
    lv_path_cofl    type STRING,
    ls_file_list    type file_info,
    lt_file_list    type STANDARD TABLE OF file_info,
    lv_count        type I,
    ls_alv_data     type typ_s_alv_data.

*   Get path separator of client OS.
    CL_GUI_FRONTEND_SERVICES=>get_file_separator( CHANGING FILE_SEPARATOR = lv_file_sep ).

*   Construct paths to data and cofiles folders.
    lv_path_data = IV_FOLDER_PATH && lv_file_sep && 'data'.
    lv_path_cofl = IV_FOLDER_PATH && lv_file_sep && 'cofiles'.

*   Check that data and cofiles folder exists.
    if CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST( lv_path_data ) <> ABAP_TRUE.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = |Data folder does not exist - { lv_path_data }|.
    endif.
    if CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST( lv_path_cofl ) <> ABAP_TRUE.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING MSG_TEXT = |Cofiles folder does not exist - { lv_path_cofl }|.
    endif.

*   Get files from data folder.
    refresh lt_file_list.
    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES(
      EXPORTING
        DIRECTORY  = lv_path_data
        FILES_ONLY = 'X'
      CHANGING
        FILE_TABLE = lt_file_list
        COUNT      = lv_count ).

*   Copy file info to ALV table.
    loop at lt_file_list into ls_file_list.
      clear ls_alv_data.
      ls_alv_data-FILE_NAME = ls_file_list-FILENAME.
      ls_alv_data-TYPE      = CON_FILE_TYPE_DATA.
      ls_alv_data-PATH_CLI  = lv_path_data && lv_file_sep && ls_file_list-FILENAME.
      append ls_alv_data to RT_ALV_DATA.
    endloop.

*   Get files from cofiles folder.
    refresh lt_file_list.
    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_LIST_FILES(
      EXPORTING
        DIRECTORY  = lv_path_cofl
        FILES_ONLY = 'X'
      CHANGING
        FILE_TABLE = lt_file_list
        COUNT      = lv_count ).

*   Copy file info to ALV table.
    loop at lt_file_list into ls_file_list.
      clear ls_alv_data.
      ls_alv_data-FILE_NAME = ls_file_list-FILENAME.
      ls_alv_data-TYPE      = CON_FILE_TYPE_COFL.
      ls_alv_data-PATH_CLI  = lv_path_cofl && lv_file_sep && ls_file_list-FILENAME.
      append ls_alv_data to RT_ALV_DATA.
    endloop.

  endmethod.

  method ADD_TARGET_INFO.
*   CHANGING
*     CT_ALV_DATA type typ_t_alv_data
*   RAISING
*     LCX_EXCEPTION,

    data:
    lx_root      type ref to CX_ROOT,
    lv_path_data type LOCALFILE,
    lv_path_cofl type LOCALFILE.

    FIELD-SYMBOLS:
    <fs_alv_data>  type typ_s_alv_data.

*   Check that data and cofiles folders exists on server.
    lv_path_data = GET_SRV_PATH_DIR_DATA( ).
    if DOES_SRV_FOLDER_EXIST( lv_path_data ) NE ABAP_TRUE.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = |Data folder does not exist on server - { lv_path_data }|.
    endif.
    lv_path_cofl = GET_SRV_PATH_DIR_COFL( ).
    if DOES_SRV_FOLDER_EXIST( lv_path_cofl ) NE ABAP_TRUE.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = |Cofiles folder does not exist on server - { lv_path_cofl }|.
    endif.

*   Add data.
    loop at CT_ALV_DATA ASSIGNING <fs_alv_data>.
      <fs_alv_data>-RC_STATUS = 0.
      TRY.
        <fs_alv_data>-PATH_SRV = GET_SRV_TRANS_FILE_PATH( EXPORTING IV_FILENAME = <fs_alv_data>-FILE_NAME   IV_TYPE = <fs_alv_data>-TYPE ).
      CATCH CX_ROOT into lx_root.
        <fs_alv_data>-RC_STATUS = 8.
        <fs_alv_data>-MESSAGE   = lx_root->GET_TEXT( ).
      ENDTRY.
      if <fs_alv_data>-RC_STATUS = 0 AND <fs_alv_data>-PATH_SRV is not initial.
*       Check if target file already exists.
        if DOES_SRV_FILE_EXIST( <fs_alv_data>-PATH_SRV ) = ABAP_TRUE.
          <fs_alv_data>-SRV_FL_EX = ABAP_TRUE.
          <fs_alv_data>-RC_STATUS = 4.
          <fs_alv_data>-MESSAGE   = 'File already exists - will be overwritten'.
        endif.
      endif.
      <fs_alv_data>-IC_STATUS = GET_STATUS_ICON( <fs_alv_data>-RC_STATUS ).
      if <fs_alv_data>-RC_STATUS = 0.
        <fs_alv_data>-MESSAGE   = 'All OK'.
      endif.
    endloop.

  endmethod.

  method GET_SRV_TRANS_FILE_PATH.
*   IMPORTING
*     IV_FILENAME  type LOCALFILE
*     IV_TYPE      type STRING
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE
*   RAISING
*     LCX_EXCEPTION,

    data:
    lv_filename  type CHAR128,
    lv_path_root type CHAR128,
    lv_path_dir  TYPE CHAR128,
    lv_path_full TYPE CHAR128.

    if IV_TYPE = LCL_TR_UPLOAD=>CON_FILE_TYPE_DATA.
      lv_path_dir = GET_SRV_PATH_DIR_DATA( ).
    elseif IV_TYPE = LCL_TR_UPLOAD=>CON_FILE_TYPE_COFL.
      lv_path_dir = GET_SRV_PATH_DIR_COFL( ).
    else.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = |Invalid type { IV_TYPE }|.
    endif.

*   Type conversion
    lv_filename = IV_FILENAME.

*   Construct path to transport data file.
    CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD lv_filename        "#EC CI_CCALL
                         ID 'PATH'     FIELD lv_path_dir
                         ID 'RESULT'   FIELD lv_path_full.
    if sy-subrc <> 0.
      RAISE EXCEPTION TYPE LCX_EXCEPTION
      EXPORTING
        MSG_TEXT = 'Path to target transport file could not be established'.
    endif.

*   Set result.
    RV_PATH = lv_path_full.

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

    data:
    lv_file_path  type CHAR128.

    lv_file_path = IV_FILE_PATH.

    open dataset lv_file_path for input
         in text mode encoding default
         ignoring conversion errors.
    IF SY-SUBRC = 0.
      RV_FILE_EXISTS = ABAP_TRUE.
    ELSE.
      RV_FILE_EXISTS = ABAP_FALSE.
    ENDIF.
    close dataset lv_file_path.

  endmethod.

  method UPLOAD_FILES.
*   CHANGING
*     CT_ALV_DATA type typ_t_alv_data,

    types:
    begin of typ_s_binary,
      LINE(1024) TYPE X,
    end of typ_s_binary,
    typ_t_binary type table of typ_s_binary.

    data:
    ls_binary   type typ_s_binary,
    lt_binary   type typ_t_binary,
    lv_cli_file type STRING,
    lv_srv_file type STRING,
    lv_file_len type I,
    lv_rest     type I,
    lv_bin_len  type I,
    lv_msg      type STRING.

    FIELD-SYMBOLS:
    <fs_alv_data>  type typ_s_alv_data,
    <fs_bin_data>  type typ_s_binary,
    <fs_binary>    type X.

*   Add files.
    loop at CT_ALV_DATA ASSIGNING <fs_alv_data> where RC_STATUS < 8.
      lv_cli_file = <fs_alv_data>-PATH_CLI.
      lv_srv_file = <fs_alv_data>-PATH_SRV.
*     Upload file content to server memory.
      call method CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
      EXPORTING
        FILENAME   = lv_cli_file
        FILETYPE   = 'BIN'
      IMPORTING
        FILELENGTH = lv_file_len
      CHANGING
        DATA_TAB   = lt_binary
      EXCEPTIONS
        OTHERS     = 99.
      if sy-subrc <> 0.
        lv_msg = ''.
        if sy-msgid is not initial AND sy-MSGNO is not initial.
          message ID sy-MSGID TYPE 'E' NUMBER sy-MSGNO into lv_msg WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        else.
          lv_msg = 'Upload of client file failed'.
        endif.
        <fs_alv_data>-RC_STATUS = 8.
        <fs_alv_data>-MESSAGE   = lv_msg.
        continue.
      endif.

*     Write file content to server file system.
      clear lv_msg.
      open dataset lv_srv_file for OUTPUT in BINARY MODE MESSAGE lv_msg.
      if sy-subrc <> 0.
        <fs_alv_data>-RC_STATUS = 8.
        <fs_alv_data>-MESSAGE   = 'Server file open error - ' && lv_msg.
        close dataset lv_srv_file.
        continue.
      endif.

*     Set missing (rest) length to be copied to file.
      lv_rest = lv_file_len.

*     Copy data to file.
      loop at lt_binary ASSIGNING <fs_bin_data>.
        assign <fs_bin_data>-LINE to <fs_binary>.
        describe field <fs_binary> length lv_bin_len in byte mode.
        check lv_rest > 0.
        if lv_rest < lv_bin_len.
*         Line contains more than needed => Copy only needed number of bytes.
          transfer <fs_binary>(lv_rest) to lv_srv_file.
          lv_rest = 0.
        else.
*         Line contains less than needed => Copy entire line and adjust rest.
          transfer <fs_binary> to lv_srv_file.
          subtract lv_bin_len from lv_rest.
        endif.
      endloop. "  Content of one file.

*     Close access to file.
      close dataset lv_srv_file.

    endloop. " List of files.

  endmethod.

  method GET_STATUS_ICON.
*   IMPORTING
*     IV_STATUS  type I
*   RETURNING
*     VALUE(RV_ICON)  type ICON_D,

    if IV_STATUS GE 8.
      RV_ICON = ICON_RED_LIGHT.
    elseif IV_STATUS GE 4.
      RV_ICON = ICON_YELLOW_LIGHT.
    else.
      RV_ICON = ICON_GREEN_LIGHT.
    endif.

  endmethod.

  method GUI_SELECT_FOLDER_F4.
*   RETURNING
*     VALUE(RV_FOLDER) type LOCALFILE,

    data:
    lv_folder  type STRING.

    CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE(
      EXPORTING
        WINDOW_TITLE    = 'Select source folder'
        INITIAL_FOLDER  = ''
      CHANGING
        SELECTED_FOLDER = lv_folder ).

    RV_FOLDER = lv_folder.
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
          when 'FILE_NAME'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'File Name' ).
          when 'TYPE'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Type' ).
          when 'PATH_CLI'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Source Path (client)' ).
          when 'PATH_SRV'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Target Path (server)' ).
          when 'SRV_FL_EX'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Overwrite' ).
            lo_col->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>CHECKBOX ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'RC_STATUS'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'RC' ).
            ls_columns-R_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
          when 'IC_STATUS'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Status' ).
            ls_columns-R_COLUMN->SET_ALIGNMENT( IF_SALV_C_ALIGNMENT=>CENTERED ).
          when 'MESSAGE'.
            ls_columns-R_COLUMN->SET_MEDIUM_TEXT( 'Message' ).
        endcase.
      endloop.

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
    CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TRANS'             "#EC CI_CCALL
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

  method GET_SRV_PATH_DIR_DATA.
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE
*   RAISING
*     LCX_EXCEPTION

    data:
    lv_path_root  type CHAR128,
    lv_path_dir   type CHAR128.

    if GV_SRV_PATH_DIR_DATA is not initial.
*     Read from cache.
      RV_PATH = GV_SRV_PATH_DIR_DATA.
    else.
*     Get root path to DIR_TRANS.
      lv_path_root = GET_SRV_PATH_DIR_TRANS( ).
*     Construct path to data folder.
      CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD 'data'           "#EC CI_CCALL
                           ID 'PATH'     FIELD lv_path_root
                           ID 'RESULT'   FIELD lv_path_dir.
      if sy-subrc <> 0.
        RAISE EXCEPTION TYPE LCX_EXCEPTION
        EXPORTING
          MSG_TEXT = 'Path to data folder could not be established'.
      endif.
*     Update cache.
      GV_SRV_PATH_DIR_DATA = lv_path_dir.
*     Set result.
      RV_PATH = lv_path_dir.
    endif.

  endmethod.

  method GET_SRV_PATH_DIR_COFL.
*   RETURNING
*     VALUE(RV_PATH) type LOCALFILE
*   RAISING
*     LCX_EXCEPTION.

    data:
    lv_path_root  type CHAR128,
    lv_path_dir   type CHAR128.

    if GV_SRV_PATH_DIR_COFL is not initial.
*     Read from cache.
      RV_PATH = GV_SRV_PATH_DIR_COFL.
    else.
*     Get root path to DIR_TRANS.
      lv_path_root = GET_SRV_PATH_DIR_TRANS( ).
*     Construct path to data folder.
      CALL 'BUILD_DS_SPEC' ID 'FILENAME' FIELD 'cofiles'        "#EC CI_CCALL
                           ID 'PATH'     FIELD lv_path_root
                           ID 'RESULT'   FIELD lv_path_dir.
      if sy-subrc <> 0.
        RAISE EXCEPTION TYPE LCX_EXCEPTION
        EXPORTING
          MSG_TEXT = 'Path to cofiles folder could not be established'.
      endif.
*     Update cache.
      GV_SRV_PATH_DIR_COFL = lv_path_dir.
*     Set result.
      RV_PATH = lv_path_dir.
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
