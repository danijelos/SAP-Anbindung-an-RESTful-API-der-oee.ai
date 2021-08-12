CLASS zcl_oee_product DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
        METHODS: set_product
                 IMPORTING
                     name TYPE string name_short TYPE string location_id TYPE string,
                 get_product_id
                 RETURNING
                     VALUE(product_id_oee) TYPE string,
                 product_post_request.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: product_name TYPE string,
          product_short TYPE string,
          product_id TYPE string,
          product_location_id TYPE string.

ENDCLASS.



CLASS zcl_oee_product IMPLEMENTATION.
    METHOD set_product.
        me->product_name = name.
        me->product_short = name_short.
        me->product_location_id = location_id.
    ENDMETHOD.

    METHOD get_product_id.
        product_id_oee = me->product_id.
    ENDMETHOD.

    METHOD product_post_request. "sending a production order post request to oee.ai

    DATA(database_com) = NEW zcl_oee_database_communication( ).

    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_rest_client TYPE REF TO cl_rest_http_client,
          http_status    TYPE string,
          type           TYPE string VALUE 'product'.

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

*   data for body -> in string format
    DATA(json_po_data) =
        `{` &&
        `"data": {` &&
            `"type": "products",` &&
            `"attributes":{` &&
                `"code":"` && me->product_short && `",` &&
                `"description":"` && product_name && `",` &&
                `"production-unit": "parts",`  &&
                `"cycle-time-mode": "parts_per_time",`  &&
                `"cycle-time-unit": "minutes",`  &&
                `"target-speed": "1",`  &&
                `"units-multiplier": "1",`  &&
                `"units-divisor": "1"`  &&
            `},` &&
            `"relationships": {` &&
                `"location": {` &&
                    `"data": {` &&
                        `"type": "locations",` &&
                        `"id":"` && me->product_location_id && `"` &&
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

    me->product_id = lv_val.

    lo_http_client->close( ).

    IF sy-subrc <> 0.
      "error handling
    ENDIF.
  ENDMETHOD.
ENDCLASS.
