#include <fcntl.h>
#include <unistd.h>
#include <pty.h>
#include <stdlib.h>
#include <string.h>
#include <vpi_user.h>
#include "receive.h"
#include "transmit.h"
#include "uart.h"

PLI_INT32 uart_create_compiletf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle;
    PLI_INT32 arg_type, arg_const_type;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);
    if (systf_handle == NULL) {
        vpi_printf("ERROR: $uart_create failed to obtain systf handle\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    if (arg_iterator == NULL) {
        vpi_printf("ERROR: $uart_create requires 1 argument\n");
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
    arg_type = vpi_get(vpiType, arg_handle);
    arg_const_type = vpi_get(vpiConstType, arg_handle);
    if ((arg_type != vpiConstant || arg_const_type != vpiStringConst) && arg_type != vpiParameter) {
        vpi_printf("ERROR: $uart_create argument 1 must be a string\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    arg_handle = vpi_scan(arg_iterator);
        if (arg_handle != NULL) {
        vpi_printf("ERROR: $uart_create can only have 1 argument\n");
        vpi_free_object(arg_iterator);
        vpi_control(vpiFinish, 0);
        return(0);
    }

    return(0);
}

PLI_INT32 uart_create_calltf(PLI_BYTE8* user_data) {
    vpiHandle systf_handle, arg_iterator, arg_handle, string_handle;
    s_vpi_value arg_value;
    uart_pty_entry_t* uart_table;

    systf_handle = vpi_handle(vpiSysTfCall, NULL);

    arg_iterator = vpi_iterate(vpiArgument, systf_handle);
    string_handle = vpi_scan(arg_iterator);
    vpi_free_object(arg_iterator);

    arg_value.format = vpiStringVal;
    vpi_get_value(string_handle, &arg_value);

    uart_add_entry_table((uart_pty_entry_t**)user_data, arg_value.value.str, uart_create(arg_value.value.str));

    return(0);
}

void* uart_create(const char* name) {
    uart_pty_t* port = (uart_pty_t*)malloc(sizeof(*port));
    
    struct termios tty;
    cfmakeraw(&tty);

    openpty(&(port->master), &(port->slave), (char*)name, &tty, NULL);
    int val = ttyname_r(port->slave, port->name, 64);
    (void) val;
    
    vpi_printf("UART at Device: %s is ready.\n", port->name);
    
    fcntl(port->master, F_SETFL, fcntl(port->master, F_GETFL, 0) | O_NONBLOCK);

    return (void*) port;
}

void uart_add_entry_table(uart_pty_entry_t** table_pointer, char* name, uart_pty_t* instance) {
    uart_pty_entry_t* pointer = *table_pointer;
    uart_pty_entry_t* entry = (uart_pty_entry_t*)malloc(sizeof(*entry));

    entry->name = name;
    entry->instance = instance;
    entry->next = NULL;

    if (pointer == NULL) {
        *table_pointer = entry;
        return;
    }

    while (pointer->next != NULL) {
        pointer = pointer->next;
    }

    pointer->next = entry;
}

uart_pty_t* uart_lookup_entry_table(uart_pty_entry_t* table, char* name) {
    while (table != NULL) {
        if (strcmp(table->name, name) == 0) {
            return table->instance;
        }
    }

    return NULL;
}

void uart_register(void) {
    s_vpi_systf_data tf_data;
    uart_pty_entry_t* table = NULL;
    uart_pty_entry_t** table_pointer = &table;

    tf_data.type = vpiSysTask;
    tf_data.sysfunctype = 0;
    tf_data.tfname = "$uart_create";
    tf_data.calltf = uart_create_calltf;
    tf_data.compiletf = uart_create_compiletf;
    tf_data.sizetf = NULL;
    tf_data.user_data = (PLI_BYTE8*)table_pointer;

    vpi_register_systf(&tf_data);

    tf_data.type = vpiSysFunc;
    tf_data.sysfunctype = vpiIntVal;
    tf_data.tfname = "$transmit_valid";
    tf_data.calltf = transmit_valid_calltf;
    tf_data.compiletf = transmit_valid_compiletf;
    tf_data.sizetf = NULL;
    tf_data.user_data = (PLI_BYTE8*)table_pointer;

    vpi_register_systf(&tf_data);

    tf_data.type = vpiSysFunc;
    tf_data.sysfunctype = vpiStringVal;
    tf_data.tfname = "$transmit_data";
    tf_data.calltf = transmit_data_calltf;
    tf_data.compiletf = transmit_data_compiletf;
    tf_data.sizetf = NULL;
    tf_data.user_data = (PLI_BYTE8*)table_pointer;

    vpi_register_systf(&tf_data);

    tf_data.type = vpiSysTask;
    tf_data.sysfunctype = 0;
    tf_data.tfname = "$receive_data";
    tf_data.calltf = receive_data_calltf;
    tf_data.compiletf = receive_data_compiletf;
    tf_data.sizetf = NULL;
    tf_data.user_data = (PLI_BYTE8*)table_pointer;

    vpi_register_systf(&tf_data);
}

void (*vlog_startup_routines[])(void) = {
    uart_register,
    0
};