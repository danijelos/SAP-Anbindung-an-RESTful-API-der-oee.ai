CLASS zcl_oee_position DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor,
             set_position
                   IMPORTING
                       po_id TYPE string location_id TYPE string product TYPE string
                       step TYPE string description TYPE string starttime TYPE string
                       batch TYPE string quantity TYPE string delta TYPE string
                       changeoverminutes TYPE string,
             set_pos_position_id
                IMPORTING pos_id TYPE string,
             position_post_request,
             position_patch_request,
             get_product_id
                 RETURNING
                     VALUE(product_id) TYPE string,
             get_position_id
                 RETURNING
                     VALUE(position_id) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: pos_po_id             TYPE string,
          pos_location_id       TYPE string,
          pos_product_id        TYPE string,
          pos_position_id       TYPE string,
          pos_step              TYPE string,
          pos_description       TYPE string,
          pos_starttime         TYPE string,
          pos_batch             TYPE string,
          pos_quantity          TYPE string,
          pos_delta             TYPE string,
          pos_changeoverminutes TYPE string.
ENDCLASS.



CLASS zcl_oee_position IMPLEMENTATION.
  METHOD constructor.
    me->pos_po_id             = 0.
    me->pos_location_id         = 0.
    me->pos_product_id          = 0.
    me->pos_step                = 0.
    me->pos_description         = 0.
    me->pos_starttime           = 0.
    me->pos_batch               = 0.
    me->pos_quantity            = 0.
    me->pos_delta               = 0.
    me->pos_changeoverminutes   = 0.
  ENDMETHOD.

  METHOD set_position. "attributes of the instance get set
    me->pos_po_id               = po_id.
    me->pos_location_id         = location_id.
    me->pos_product_id          = product.
    me->pos_step                = step.
    me->pos_description         = description.
    me->pos_starttime           = starttime.
    me->pos_batch               = batch.
    me->pos_quantity            = quantity.
    me->pos_delta               = delta.
    me->pos_changeoverminutes   = changeoverminutes.
  ENDMETHOD.

  METHOD set_pos_position_id. "position id gets set
    me->pos_position_id = pos_id.
  ENDMETHOD.

  METHOD get_product_id. "get product_id
    product_id = me->pos_product_id.
  ENDMETHOD.

  METHOD get_position_id. "get position_id
    position_id = me->pos_position_id.
  ENDMETHOD.

  METHOD position_post_request. "POST Request for position

    DATA(database_com) = NEW zcl_oee_database_communication( ).

    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_rest_client TYPE REF TO cl_rest_http_client,
          http_status    TYPE string,
          type           TYPE string VALUE 'position'.

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
*   data for the body -> in string format



    DATA(json_position_data) =
        `{` &&
        `"data": {` &&
            `"type": "line-items",` &&
            `"attributes":{` &&
                `"step":"` && me->pos_step && `",` &&
                `"description":"` && me->pos_description && `",` &&
                `"quantity":"` && me->pos_quantity && `",` &&
                `"delta":"` && me->pos_delta && `",` &&
                `"start-time":"` && me->pos_starttime && `",` &&
                `"changeover-minutes":"` && me->pos_changeoverminutes && `",` &&
                `"batch":"` && me->pos_batch && `"` &&
        `},` &&
        `"relationships": {` &&
            `"location": {` &&
                `"data": {` &&
                    `"type": "locations",` &&
                    `"id":"` && me->pos_location_id && `"` &&
                `}` &&
            `},` &&
            `"production-order": {` &&
                `"data": {` &&
                    `"type": "production-orders",` &&
                    `"id":"` && me->pos_po_id && `"` &&
                `}` &&
            `},` &&
            `"product": {` &&
                `"data": {` &&
                    `"type": "products",` &&
                    `"id":"` && me->pos_product_id && `"` &&
                `}` &&
            `}` &&
        `}` &&
        `}` &&
        `}`.

*   Set Header and Body
    lo_http_client->request->set_method( 'POST' ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/vnd.api+json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
    lo_http_client->request->append_cdata( json_position_data ).

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
    me->pos_position_id = lv_val.

    lo_http_client->close( ).

    IF sy-subrc <> 0.
      "error handling
    ENDIF.
  ENDMETHOD.

  METHOD position_patch_request. "PATCH Request for updating a position

    DATA(database_com) = NEW zcl_oee_database_communication( ).
    DATA: lo_http_client TYPE REF TO if_http_client,
          lo_rest_client TYPE REF TO cl_rest_http_client,
          http_status    TYPE string,
          type           TYPE string VALUE 'position'.

    DATA(lv_url) = database_com->get_url( type ) && me->pos_position_id.
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
*   data for the body -> in string format
    DATA(json_position_data) =
        `{` &&
        `"data": {` &&
            `"id":"` && me->pos_position_id && `",` &&
            `"type": "line-items",` &&
            `"attributes":{` &&
                `"step":"` && me->pos_step && `",` &&
                `"description":"` && me->pos_description && `",` &&
                `"quantity":"` && me->pos_quantity && `",` &&
                `"delta":"` && me->pos_delta && `",` &&
                `"start-time":"` && me->pos_starttime && `",` &&
                `"changeover-minutes":"` && me->pos_changeoverminutes && `",` &&
                `"batch":"` && me->pos_batch && `"` &&
        `},` &&
        `"relationships": {` &&
            `"location": {` &&
                `"data": {` &&
                    `"type": "locations",` &&
                    `"id":"` && me->pos_location_id && `"` &&
                `}` &&
            `},` &&
            `"production-order": {` &&
                `"data": {` &&
                    `"type": "production-orders",` &&
                    `"id":"` && me->pos_po_id && `"` &&
                `}` &&
            `},` &&
            `"product": {` &&
                `"data": {` &&
                    `"type": "products",` &&
                    `"id":"` && me->pos_product_id && `"` &&
                `}` &&
            `}` &&
        `}` &&
        `}` &&
        `}`.

*   Set Header and Body
    lo_http_client->request->set_method( 'PATCH' ).
    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/vnd.api+json' ).
    lo_http_client->request->set_header_field( name = 'Authorization' value = lv_auth ).
    lo_http_client->request->append_cdata( json_position_data ).

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
    me->pos_position_id = lv_val.
    lo_http_client->close( ).

    IF sy-subrc <> 0.
      "error handling
    ENDIF.
  ENDMETHOD.

ENDCLASS.
