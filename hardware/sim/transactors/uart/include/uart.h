#include <vpi_user.h>

typedef struct {
    char name[64];
    int master;
    int slave;
    char data;
} uart_pty_t;

typedef struct uart_pty_entry {
    char* name;
    uart_pty_t* instance;
    struct uart_pty_entry* next;
} uart_pty_entry_t;

PLI_INT32 uart_create_compiletf(PLI_BYTE8* user_data);
PLI_INT32 uart_create_calltf(PLI_BYTE8* user_data);

void* uart_create(const char* name);

void uart_add_entry_table(uart_pty_entry_t** table_pointer, char* name, uart_pty_t* instance);
uart_pty_t* uart_lookup_entry_table(uart_pty_entry_t* table, char* name);