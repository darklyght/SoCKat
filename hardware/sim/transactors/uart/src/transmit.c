#include <vpi_user.h>
#include <unistd.h>
#include "transmit.h"
#include "uart.h"

PLI_INT32 transmit_valid_compiletf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle;
    PLI_INT32 arg_type, arg_const_type;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);
    if (systf_handle == NULL) {
        vpi_printf("ERROR: $transmit_valid failed to obtain systf handle\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    if (arg_iterator == NULL) {
        vpi_printf("ERROR: $transmit_valid requires 1 argument\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
    arg_type = vpi_get(vpiType, arg_handle);
    arg_const_type = vpi_get(vpiConstType, arg_handle);
    if ((arg_type != vpiConstant || arg_const_type != vpiStringConst) && arg_type != vpiParameter) {
        vpi_printf("ERROR: $transmit_valid argument 1 must be a string\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
        if (arg_handle != NULL) {
        vpi_printf("ERROR: $transmit_valid can only have 1 argument\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    return(0);
}

PLI_INT32 transmit_valid_calltf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle, string_handle;
    s_vpi_value arg_value, ret_value;
    uart_pty_t* instance;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    string_handle = vpi_scan(arg_iterator);
    vpi_free_object(arg_iterator);

    arg_value.format = vpiStringVal;
    vpi_get_value(string_handle, &arg_value);

    instance = uart_lookup_entry_table(*(uart_pty_entry_t**)user_data, arg_value.value.str);
    
    ret_value.format = vpiIntVal;
    ret_value.value.integer = read(((uart_pty_t*)instance)->master, &(((uart_pty_t*)instance)->data), 1) == 1;

    vpi_put_value(systf_handle, &ret_value, NULL, vpiNoDelay);

    return(0);
}

PLI_INT32 transmit_data_compiletf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle;
    PLI_INT32 arg_type, arg_const_type;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);
    if (systf_handle == NULL) {
        vpi_printf("ERROR: $transmit_data failed to obtain systf handle\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    if (arg_iterator == NULL) {
        vpi_printf("ERROR: $transmit_data requires 1 argument\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
    arg_type = vpi_get(vpiType, arg_handle);
    arg_const_type = vpi_get(vpiConstType, arg_handle);
    if ((arg_type != vpiConstant || arg_const_type != vpiStringConst) && arg_type != vpiParameter) {
        vpi_printf("ERROR: $transmit_data argument 1 must be a string\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
        if (arg_handle != NULL) {
        vpi_printf("ERROR: $transmit_data can only have 1 argument\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    return(0);
}

PLI_INT32 transmit_data_calltf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle, string_handle;
    s_vpi_value arg_value, ret_value;
    uart_pty_t* instance;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    string_handle = vpi_scan(arg_iterator);
    vpi_free_object(arg_iterator);

    arg_value.format = vpiStringVal;
    vpi_get_value(string_handle, &arg_value);

    instance = uart_lookup_entry_table(*(uart_pty_entry_t**)user_data, arg_value.value.str);
    
    ret_value.format = vpiStringVal;
    ret_value.value.str = &((uart_pty_t*)instance)->data;
    vpi_put_value(systf_handle, &ret_value, NULL, vpiNoDelay);

    return(0);
}