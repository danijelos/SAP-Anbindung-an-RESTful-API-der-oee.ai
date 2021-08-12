CLASS zcl_oee_productionorder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor,
             set_productionorder
                 IMPORTING
                     code TYPE string description TYPE string starttime TYPE string
                     priority TYPE string details TYPE string location TYPE string,
             set_po_po_id
                IMPORTING po_id TYPE string,
             productionorder_post_request,
             productionorder_patch_request,
             get_productionorder_id
                 RETURNING
                     VALUE(po_id) TYPE string,
             get_location_id
                 RETURNING
                     VALUE(location_id) TYPE string,
             json_serialize
                 RETURNING
                    VALUE(test) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: po_code        TYPE string,
          po_description TYPE string,
          po_starttime   TYPE string,
          po_priority    TYPE string,
          po_details     TYPE string,
          po_location    TYPE string,
          po_po_id     TYPE string.
ENDCLASS.



CLASS zcl_oee_productionorder IMPLEMENTATION.

  METHOD constructor.
    me->po_code           = 0.
    me->po_description    = 0.
    me->po_starttime      = 0.
    me->po_priority       = 0.
    me->po_details        = 0.
    me->po_location       = 0.
    me->po_po_id        = 0.
  ENDMETHOD.

  METHOD set_productionorder. "attributes of the instance get set
    me->po_code           = code.
    me->po_description    = description.
    me->po_starttime      = starttime.
    me->po_priority       = priority.
    me->po_details        = details.
    me->po_location       = location.
  ENDMETHOD.

  METHOD set_po_po_id. "set production order ID
    me->po_po_id = po_id.
  ENDMETHOD.

  METHOD get_productionorder_id. "get production order ID
    po_id = me->po_po_id.
  ENDMETHOD.

  METHOD get_location_id. "get location ID
    location_id = me->po_location.
  ENDMETHOD.

  METHOD productionorder_post_request. "sending a production order post request to oee.ai

    DATA(database_com) = NEW zcl_oee_database_communication( ).

    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_rest_client TYPE REF TO cl_rest_http_client,
          http_status    TYPE string,
          type           TYPE string VALUE 'productionorder'.

    DATA(lv_auth) = database_com->get_token( type ). "token

    cl_http_client=>create_by_url(
        EXPORTING
            url = database_com->get_url( type )
        IMPORTING
            client = lo_http_client "HTTP Client Abstraction
        EXCEPTIONS
            argument_not_found = 1

            plugin_not_active  = 2

            internal_error     = 3

            OTHERS             = 4
    ).

*    TYPES: BEGIN OF location_data_request,
*           id TYPE string,
*           type TYPE string,
*           END OF location_data_request.

*    TYPES: data1 TYPE STANDARD TABLE OF location_data_request WITH NON-UNIQUE DEFAULT KEY.

*    TYPES: BEGIN OF location1,
*            data1 TYPE data1,
*           END OF location1.

*    TYPES: location TYPE STANDARD TABLE OF location1 WITH NON-UNIQUE DEFAULT KEY.

*    TYPES: BEGIN OF relationships1,
*           location TYPE location,
*           END OF relationships1.

*    TYPES relationships TYPE STANDARD TABLE OF relationships1 WITH NON-UNIQUE DEFAULT KEY.

*    TYPES: BEGIN OF attribute_request,
*           code TYPE string,
*           description TYPE string,
*           starttime TYPE string,
*           priority TYPE string,
*           details TYPE string,
*           END OF attribute_request.

*    TYPES attributes TYPE STANDARD TABLE OF attribute_request WITH NON-UNIQUE DEFAULT KEY.

*    TYPES: BEGIN OF data2,
*           type TYPE string,
*           attributes TYPE attributes,
*           relationships TYPE relationships,
*           END OF data2.

*
*    DATA: request_body TYPE data2,
*          attributes_body TYPE attribute_request,
*          location_body TYPE location_data_request.
*
*    DATA response_test TYPE string.
*
*    request_body = VALUE data2( type = 'production-order'
*                                attributes = VALUE #( code = me->po_code
*                                                      description = me->po_description
*                                                      starttime = me->po_starttime
*                                                      priority = me->po_priority
*                                                      details = me->po_details )
*                                relationships = VALUE #(
*                                 location = VALUE #(
*                                  data = VALUE #(
*                                   type = 'locations'
*                                   id = me->po_location )
*                                   )
*                           ).



*    DATA response_abap TYPE data1.
*    REPLACE 'data1' IN response1 WITH 'data'.

*   data for body -> in string format
    DATA(json_po_data) =
        `{` &&
        `"data": {` &&
            `"type": "production-orders",` &&
            `"attributes":{` &&
                `"code":"` && me->po_code && `",` &&
                `"description":"` && me->po_description && `",` &&
                `"start-time":"` && me->po_starttime && `",` &&
                `"priority":"` && me->po_priority && `",` &&
                `"details":"` && me->po_details && `"` &&
            `},` &&
            `"relationships": {` &&
                `"location": {` &&
                    `"data": {` &&
                        `"type": "locations",` &&
                        `"id":"` && me->po_location && `"` &&
                    `}` &&
                `}` &&
            `}` &&
        `}` &&
        `}`.

*   Set Header and Body
    lo_http_client->request->set_method( 'POST' ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/vnd.api+json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
    lo_http_client->request->append_cdata( json_po_data ).

    lo_http_client->send(
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5 ).

    IF sy-subrc = 0.
      lo_http_client->receive(
          EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              OTHERS                     = 5 ).
    ENDIF.


*   Getting the ID of the Production Order
    DATA: lv_json TYPE /ui2/cl_json=>json,
          lr_data TYPE REF TO data,
          lv_val TYPE string.

    FIELD-SYMBOLS: <data> TYPE data,
               <struct> TYPE any,
               <field> TYPE any.

    lv_json = lo_http_client->response->get_cdata( ).
    lr_data = /ui2/cl_json=>generate( json = lv_json ).

    /ui2/cl_data_access=>create( ir_data = lr_data iv_component = `data-id`)->value( IMPORTING ev_data = lv_val ).

    me->po_po_id = lv_val.

    lo_http_client->close( ).

    IF sy-subrc <> 0.
      "error handling
    ENDIF.
  ENDMETHOD.

  METHOD productionorder_patch_request. "updating a production order per patch request

    DATA(database_com) = NEW zcl_oee_database_communication( ).
    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_rest_client TYPE REF TO cl_rest_http_client,
          http_status    TYPE string,
          type           TYPE string VALUE 'productionorder'.

    DATA(lv_url) = database_com->get_url( type ) && me->po_po_id. "API URL

    DATA(lv_auth) = database_com->get_token( type ). "token

    cl_http_client=>create_by_url(
        EXPORTING
            url = lv_url
        IMPORTING
            client = lo_http_client "HTTP Client Abstraction
        EXCEPTIONS
            argument_not_found = 1

            plugin_not_active  = 2

            internal_error     = 3

            OTHERS             = 4
    ).
*   data for body -> in string format
    DATA(json_po_data) =
        `{` &&
        `"data": {` &&
            `"id":"` && me->po_po_id && `",` &&
            `"type": "production-orders",` &&
            `"attributes":{` &&
                `"code":"` && me->po_code && `",` &&
                `"description":"` && me->po_description && `",` &&
                `"start-time":"` && me->po_starttime && `",` &&
                `"priority":"` && me->po_priority && `",` &&
                `"details":"` && me->po_details && `"` &&
        `},` &&
        `"relationships": {` &&
            `"location": {` &&
                `"data": {` &&
                    `"type": "locations",` &&
                    `"id":"` && me->po_location && `"` &&
                `}` &&
            `}` &&
        `}` &&
        `}` &&
        `}`.

*   Set Header and Body
    lo_http_client->request->set_method( 'PATCH' ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/vnd.api+json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
    lo_http_client->request->append_cdata( json_po_data ).

    lo_http_client->send(
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            http_invalid_timeout       = 4
            OTHERS                     = 5 ).

    IF sy-subrc = 0.
      lo_http_client->receive(
          EXCEPTIONS
              http_communication_failure = 1
              http_invalid_state         = 2
              http_processing_failed     = 3
              OTHERS                     = 5 ).
    ENDIF.


*   Getting the ID of the Production Order

    DATA: lv_json TYPE /ui2/cl_json=>json,
          lr_data TYPE REF TO data,
          lv_val TYPE string.

    FIELD-SYMBOLS: <data> TYPE data,
               <struct> TYPE any,
               <field> TYPE any.

    lv_json = lo_http_client->response->get_cdata( ).
    lr_data = /ui2/cl_json=>generate( json = lv_json ).

    /ui2/cl_data_access=>create( ir_data = lr_data iv_component = `data-id`)->value( IMPORTING ev_data = lv_val ).

    me->po_po_id = lv_val.
    lo_http_client->close( ).

    IF sy-subrc <> 0.
      "error handling
    ENDIF.

  ENDMETHOD.

  METHOD json_serialize.




  ENDMETHOD.

ENDCLASS.
