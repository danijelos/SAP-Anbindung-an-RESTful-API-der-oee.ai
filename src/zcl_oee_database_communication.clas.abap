CLASS zcl_oee_database_communication DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:    po_po_id_mapping
                    IMPORTING
                        po_code TYPE string
                    RETURNING
                        VALUE(productionorder_id) TYPE string,
                location_mapping
                    IMPORTING
                        location TYPE string
                    RETURNING
                        VALUE(locationid) TYPE string,
                product_mapping
                    IMPORTING
                        product TYPE string
                    RETURNING
                        VALUE(productid) TYPE string,
                pos_position_id_mapping
                    IMPORTING
                        po_code TYPE string
                    RETURNING
                        VALUE(position_id) TYPE string,
                productionorder_check
                    IMPORTING
                        po_code TYPE string
                    RETURNING
                        VALUE(is_found) TYPE abap_bool,
                product_check
                    IMPORTING
                        product_code TYPE string
                    RETURNING
                        VALUE(is_found) TYPE abap_bool,
                insert_productionorder
                    IMPORTING
                        po_client TYPE string po_code TYPE string po_description TYPE string po_starttime TYPE string po_priority TYPE string
                        po_details TYPE string po_location TYPE string po_step TYPE string po_batch TYPE string po_quantity TYPE string
                        po_delta TYPE string po_changeoverminutes TYPE string po_product TYPE string po_product_id_oee TYPE string
                        po_location_id_oee TYPE string po_po_id_oee TYPE string po_position_id_oee TYPE string po_lastchangeddate TYPE string,
                insert_product
                    IMPORTING
                        product_client TYPE string product_short TYPE string product_long TYPE string product_id_oee TYPE string,
                update_productionorder
                    IMPORTING
                        po_code TYPE string
                        po_starttime TYPE string
                        po_quantity TYPE string
                        po_lastchangeddate TYPE string,
                get_sm59_id
                    IMPORTING
                        type TYPE string
                    RETURNING
                        VALUE(sm59_id) TYPE string,
                get_token
                    IMPORTING
                        type TYPE string
                    RETURNING
                        VALUE(auth_token) TYPE string,
                get_url
                    IMPORTING
                        type TYPE string
                    RETURNING
                        VALUE(url) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_oee_database_communication IMPLEMENTATION.

  METHOD po_po_id_mapping. "mapping the production order ID at the zcreatedpo table
    SELECT SINGLE productionorder_id_oee FROM zcreatedpo WHERE code = @po_code INTO @productionorder_id.
  ENDMETHOD.

  METHOD location_mapping. "mapping the location ID at the zlocationmap table
    SELECT SINGLE location_id_oee FROM zlocationmap WHERE location_short = @location INTO @locationid.
  ENDMETHOD.

  METHOD pos_position_id_mapping. "mapping the position ID at the zcreatedpo table
    SELECT SINGLE position_id_oee FROM zcreatedpo WHERE code = @po_code INTO @position_id.
  ENDMETHOD.

  METHOD product_mapping. "mapping the product ID at the zproductmap table
*   product-mapping: id is pulled that the production has in oee.ai for the insert
    SELECT SINGLE product_id_oee FROM zproductmap WHERE product_short = @product INTO @productid.
  ENDMETHOD.

  METHOD productionorder_check. "check if production order was send to oee.ai already or not
* for the case that there is production orders already that were not send because the program was implemented later
    DATA help TYPE string.
    is_found = abap_true.
    SELECT SINGLE productionorder_id_oee FROM zcreatedpo WHERE code = @po_code INTO @help.
    IF help IS INITIAL.
      is_found = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD product_check. "check if product order was send to oee.ai already or not
* for the case that there is production orders already that were not send because the program was implemented later
    DATA help TYPE string.
    is_found = abap_true.
    SELECT SINGLE product_short FROM zproductmap WHERE product_short = @product_code INTO @help.
    IF help IS INITIAL.
      is_found = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD insert_productionorder.
*     INSERT created production order into database for purposes of Update and Delete
    DATA: lt_productionorder TYPE TABLE OF zcreatedpo.

    lt_productionorder = VALUE #(
      ( client = po_client code = po_code description = po_description starttime = po_starttime priority = po_priority
      details = po_details location = po_location step = po_step batch = po_batch quantity = po_quantity
      delta = po_delta changeoverminutes = po_changeoverminutes product = po_product product_id_oee = po_product_id_oee
      location_id_oee = po_location_id_oee productionorder_id_oee = po_po_id_oee position_id_oee = po_position_id_oee lastchangeddate = po_lastchangeddate )
    ).
    CALL FUNCTION 'ENQUEUE_EZ_LOCK_ZCRPO'.
    INSERT zcreatedpo FROM TABLE @lt_productionorder.
    DATA(x) = 1 + 1.
    CALL FUNCTION 'DEQUEUE_EZ_LOCK_ZCRPO'.
  ENDMETHOD.

  METHOD insert_product.
*     INSERT created production order into database for purposes of Update and Delete
    DATA: lt_product TYPE TABLE OF zproductmap.

    lt_product = VALUE #(
      ( client = product_client product = product_long product_short = product_short product_id_oee = product_id_oee )
    ).
    CALL FUNCTION 'ENQUEUE_EZ_LOCK_PROMAP'.
    INSERT zproductmap FROM TABLE @lt_product.
    DATA(x) = 1 + 1.
    CALL FUNCTION 'DEQUEUE_EZ_LOCK_PROMAP'.
  ENDMETHOD.

  METHOD update_productionorder.
*     UPDATE created production order into database for purposes of Update and Delete
    CALL FUNCTION 'ENQUEUE_EZ_LOCK_ZCRPO'.
    UPDATE zcreatedpo SET starttime = po_starttime
                          quantity = po_quantity
                          lastchangeddate = po_lastchangeddate WHERE code = po_code.
    CALL FUNCTION 'DEQUEUE_EZ_LOCK_ZCRPO'.
  ENDMETHOD.

  METHOD get_sm59_id. "mapping the product ID at the zproductmap table
*   product-mapping: id is pulled that the production has in oee.ai for the insert
    SELECT SINGLE sm59_conid FROM zapiconnection WHERE type = @type INTO @sm59_id.
  ENDMETHOD.

  METHOD get_token. "mapping the product ID at the zproductmap table
*   product-mapping: id is pulled that the production has in oee.ai for the insert
    SELECT SINGLE token FROM zapiconnection WHERE type = @type INTO @auth_token.
  ENDMETHOD.

  METHOD get_url. "mapping the product ID at the zproductmap table
*   product-mapping: id is pulled that the production has in oee.ai for the insert
    SELECT SINGLE url FROM zapiconnection WHERE type = @type INTO @url.
  ENDMETHOD.

ENDCLASS.
