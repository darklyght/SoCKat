#include <vpi_user.h>
#include <unistd.h>
#include "receive.h"
#include "uart.h"

PLI_INT32 receive_data_compiletf(PLI_UBYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle;
    PLI_INT32 arg_type, arg_const_type;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);
    if (systf_handle == NULL) {
        vpi_printf("ERROR: $receive_data failed to obtain systf handle\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    if (arg_iterator == NULL) {
        vpi_printf("ERROR: $receive_data requires 2 arguments\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
    arg_type = vpi_get(vpiType, arg_handle);
    arg_const_type = vpi_get(vpiConstType, arg_handle);
    if ((arg_type != vpiConstant || arg_const_type != vpiStringConst) && arg_type != vpiParameter) {
        vpi_printf("ERROR: $receive_data argument 1 must be a string\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
    if (arg_iterator == NULL) {
        vpi_printf("ERROR: $receive_data requires 2 arguments\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }
    arg_type = vpi_get(vpiType, arg_handle);
    if (arg_type != vpiNet) {
        vpi_printf("ERROR: $receive_data argument 2 must be a net\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
        if (arg_handle != NULL) {
        vpi_printf("ERROR: $receive_data can only have 1 argument\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    return(0);
}

PLI_INT32 receive_data_calltf(PLI_UBYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle, string_handle, net_handle;
    s_vpi_value arg_value, ret_value;
    uart_pty_t* instance;
    char data;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    string_handle = vpi_scan(arg_iterator);
    net_handle = vpi_scan(arg_iterator);
    vpi_free_object(arg_iterator);

    arg_value.format = vpiStringVal;
    vpi_get_value(string_handle, &arg_value);

    instance = uart_lookup_entry_table(*(uart_pty_entry_t**)user_data, arg_value.value.str);

    arg_value.format = vpiIntVal;
    vpi_get_value(net_handle, &arg_value);

    data = (char)arg_value.value.integer;

    vpi_printf("UART received: %02X (", data);
    print_char(data);
    vpi_printf(")\n");

    write(((uart_pty_t*)instance)->master, &data, 1);

    return(0);
}

void print_char(char c) {
    switch (c) {
        case '\n':
            vpi_printf("\\n");
            break;
        case '\r':
            vpi_printf("\\r");
            break;
        case '\t':
            vpi_printf("\\t");
            break;
        default:
            if ((c < 0x20) || (c > 0x7f)) {
                vpi_printf("\\%03o", c);
            } else {
                vpi_printf("%c", c);
            }
            break;
    }
}