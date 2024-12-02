FUNCTION zfm_gp_sd_erp_print_dq.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(TMPLID) TYPE  STRING
*"     VALUE(ZHANGID) TYPE  STRING OPTIONAL
*"     VALUE(DATAJSON) TYPE  STRING
*"     VALUE(OSSKEY) TYPE  STRING
*"     VALUE(PROFILENAME) TYPE  STRING DEFAULT 'prod'
*"  EXPORTING
*"     VALUE(URL) TYPE  STRING
*"     VALUE(ERPJSON) TYPE  STRING
*"     VALUE(ERPBACK) TYPE  STRING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_GP_SD_ERP_PRINT_DQ'.
  zfmdatasave2 'B'.
  COMMIT WORK.
*
*  TYPES : BEGIN OF ZSERPJG,
*            PDFID   TYPE STRING,
*            PDFNAME TYPE STRING,
*            ZHU     TYPE STRING,
*            AREA    TYPE STRING,
*            LIST    TYPE STRING,
*            LIST2   TYPE STRING,
*          END OF ZSERPJG.
  TYPES : BEGIN OF zserpback,
            url TYPE string,
          END OF zserpback.
  TYPES : BEGIN OF zserpfail,
            msg TYPE string,
          END OF zserpfail.
  TYPES : BEGIN OF zserpjg,
            tmplid   TYPE string,
            zhangid  TYPE string,
            datajson TYPE string,
            osskey   TYPE string,
          END OF zserpjg.
  DATA: erpdata   TYPE zserpjg.
  DATA: erpfail   TYPE zserpfail.
  DATA: itre      TYPE zserpback.
  DATA: erpurl    TYPE string.
  DATA: erpmsg    TYPE string.
  DATA: erpsta    TYPE i.
*  DATA: ERPSTR_RE TYPE STRING.

  erpdata-tmplid   = tmplid    .
  CONDENSE erpdata-tmplid   .
  erpdata-zhangid  = zhangid   .
  CONDENSE erpdata-zhangid  .
  erpdata-datajson = datajson  .
  CONDENSE erpdata-datajson .
  erpdata-osskey   = osskey    .
  CONDENSE erpdata-osskey   .

  erpjson = `{"tmplid":"`   && erpdata-tmplid &&
            `", "zhangid" : "`   && erpdata-zhangid &&
            `","dataJSON":`   && erpdata-datajson &&
            `,"osskey":"`   && erpdata-osskey &&
            `","profileName":"`   && profilename &&
            `"} `
            .

  PERFORM getdata(zpub_data) USING 'ZFM_GP_SD_ERP_PRINT_DQ' CHANGING erpurl.
  IF erpurl IS INITIAL.
    rtype = 'N'.
    rtmsg = 'ERP打印服务URL未维护！'.
    zfmdatasave2 'R'.
    RETURN.
  ENDIF.

*调用函数HTTP

  PERFORM replace(zpubform) USING 'https' 'http' CHANGING erpurl.
  CALL FUNCTION 'ZFMS_15_HTTP'
    EXPORTING
      input     = erpjson
      url       = erpurl
      reqmethod = 'POST' "HTTP 方法
      http1_1   = 'X' "协议1.1/1.0
    IMPORTING
      output    = erpback "返回JSON报文
      rtmsg     = erpmsg "消息
      status    = erpsta "HTTP状态
    EXCEPTIONS
      OTHERS    = 1.


  IF erpsta = 200.
    /ui2/cl_json=>deserialize( EXPORTING json = erpback pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = itre ).
    url = itre-url.

    IF url IS NOT INITIAL  AND ( url(4) = 'HTTP' OR  url(4) = 'http' ).
      rtype = 'S'.
      rtmsg = '成功！'.
    ELSE.
      /ui2/cl_json=>deserialize( EXPORTING json = erpback pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = erpfail ).
      rtype = 'E'.
      rtmsg = '失败！' && erpfail-msg.
    ENDIF.

  ELSE.
    /ui2/cl_json=>deserialize( EXPORTING json = erpback pretty_name = /ui2/cl_json=>pretty_mode-camel_case CHANGING data = erpfail ).

    rtype = 'E'.
    rtmsg = '失败！' && erpfail-msg.
  ENDIF.
