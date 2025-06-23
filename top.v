`timescale 1ns / 1ps

module top #(
    parameter DATA_WIDTH = 16,
    parameter USERS = 2 * 16,
    parameter ANTENNAS = 2 * 16 
    )(
        input clk,
        input rst,
        input [DATA_WIDTH-1:0] data_in,
        input [DATA_WIDTH-1:0] addr,
        input reading_channel_state,
        input reading_symbols,
        input [1:0] set,
        input start_calculation,
        input [31:0] modulation,
        input [2:0] nmax,
        input [2:0] mmax,
        input [31:0] total_outputs,
        output reg CI_precoded,
	output [ANTENNAS*DATA_WIDTH-1:0] tr_out
    );
    
    wire [DATA_WIDTH-1:0] row_CICS_1, col_CICS_1, gradient_index_CICS_1;
    wire [DATA_WIDTH-1:0] S_out_CICS_1, S_out_GBCD_1, H_out_1, Ginv_out_CICS_1, Ginv_out_GBCD_1, g_mem_output_0_CICS_1, g_mem_output_1_CICS_1;
    wire g_written_0, gradient_enable_1, update_step_g_CICS_1, CICS_completed_1;
    wire [DATA_WIDTH*USERS-1:0] step_g_CICS_1;
    wire [DATA_WIDTH-1:0] row_GBCD_1, col_GBCD_1, gradient_index_GBCD_1, g_mem_output_0_GBCD_1, g_mem_output_1_GBCD_1;
    wire update_step_g_GBCD_1, GBCD_completed_1;
    wire [DATA_WIDTH*USERS-1:0] step_g_GBCD_1;
    wire reset_values_CICS_1, reset_values_GBCD_1;
    wire i_selector_CICS_1, i_selector_GBCD_1;
    wire g_written_1;
    wire not_busy_GBCD_1, CICS_waiting_1;
    wire chance_1,reset_output_1,can_trigger_1,crossed_1;
    integer no_output1;
    
    wire [DATA_WIDTH-1:0] row_CICS_2, col_CICS_2, g_mem_output_0_CICS_2, g_mem_output_1_CICS_2, gradient_index_CICS_2;
    wire [DATA_WIDTH-1:0] S_out_CICS_2, S_out_GBCD_2, H_out_2, Ginv_out_CICS_2,  Ginv_out_GBCD_2, g_mem_output_0_GBCD_2, g_mem_output_1_GBCD_2;   
    wire g_written_2, gradient_enable_2, update_step_g_CICS_2, CICS_completed_2;
    wire [DATA_WIDTH*USERS-1:0] step_g_CICS_2;
    wire [DATA_WIDTH-1:0] row_GBCD_2, col_GBCD_2, gradient_index_GBCD_2;
    wire update_step_g_GBCD_2, GBCD_completed_2;
    wire [DATA_WIDTH*USERS-1:0] step_g_GBCD_2;
    wire reset_values_CICS_2, reset_values_GBCD_2;
    wire i_selector_CICS_2, i_selector_GBCD_2;
    wire g_written_3;
    wire not_busy_GBCD_2,CICS_waiting_2;
    wire chance_2,reset_output_2,can_trigger_2,crossed_2;
    integer no_output2;
    
    reg [DATA_WIDTH-1:0] row, col;
    wire [DATA_WIDTH-1:0] g_mem_output_0, g_mem_output_1, g_mem_output_2, g_mem_output_3;
    
    wire chance,trigger;
    
    wire output_selector;
    
    reg start_calculation_2;
    
    reg firstTime;
    
    wire more_outputs;

    wire [DATA_WIDTH*USERS-1:0] entire_column_CICS_1,entire_column_CICS_2,entire_column_GBCD_1,entire_column_GBCD_2,entire_symbols_CICS_1,entire_symbols_CICS_2,entire_symbols_GBCD_1,entire_symbols_GBCD_2;
    
    
    assign output_selector = chance ? GBCD_completed_2 : GBCD_completed_1; 
    assign chance = chance_1 + chance_2;
    assign trigger = can_trigger_1 + can_trigger_2;
    assign more_outputs = total_outputs > no_output1 + no_output2;
    
    CICS #
    (
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS),
        .ANTENNAS(ANTENNAS)
    )
    CICS_UNIT_1
    (
        .clk(clk),
        .rst(rst),
        .modulation(modulation),
        .start_calculation((start_calculation & firstTime) | (crossed_2 & !trigger & more_outputs)),
        .nmax(nmax),
        .row(row_CICS_1),
        .col(col_CICS_1),
        .S_out(S_out_CICS_1),
        .Ginv_out(Ginv_out_CICS_1),
        .g_mem_output_0(g_mem_output_0_CICS_1),
        .g_mem_output_1(g_mem_output_1_CICS_1),
        .g_written_0(g_written_0),
        .g_written_1(g_written_1),
        .gradient_enable(gradient_enable_1),
        .gradient_index(gradient_index_CICS_1),
        .step_g(step_g_CICS_1),
        .update_step_g(update_step_g_CICS_1),
        .completed(CICS_completed_1),
        .reset_values(reset_values_CICS_1),
        .i_selector(i_selector_CICS_1),
        .GBCD_not_busy(not_busy_GBCD_1),
        .waiting(CICS_waiting_1),
        .can_trigger(can_trigger_1),
        .crossed(crossed_1),
        .entire_column(entire_column_CICS_1),
        .entire_symbols(entire_symbols_CICS_1)
    );


    GBCD #
    (
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS),
        .ANTENNAS(ANTENNAS)
    )
    GBCD_UNIT_1
    (
        .clk(clk),
        .rst(rst),
        .modulation(modulation),
        .start_calculation(CICS_completed_1),
        .mmax(mmax),
        .row(row_GBCD_1),
        .col(col_GBCD_1),
        .S_out(S_out_GBCD_1),
        .Ginv_out(Ginv_out_GBCD_1),
        .g_mem_output_0(g_mem_output_0_GBCD_1),
        .g_mem_output_1(g_mem_output_1_GBCD_1),
        .g_written_0(g_written_0),
        .g_written_1(g_written_1),
        .gradient_index(gradient_index_GBCD_1),
        .step_g(step_g_GBCD_1),
        .update_step_g(update_step_g_GBCD_1),
        .completed(GBCD_completed_1),
        .reset_values(reset_values_GBCD_1),
        .i_selector(i_selector_GBCD_1),
        .chance(!chance),
        .precoded(CI_precoded),
        .not_busy(not_busy_GBCD_1),
        .track(chance_1),
        .reset_output(reset_output_1),
        .entire_column(entire_column_GBCD_1),
        .entire_symbols(entire_symbols_GBCD_1)
    );
    
    input_storage #(
    .DATA_WIDTH(DATA_WIDTH),
    .MATRIX_SIZE(USERS),
    .USERS(USERS),
    .ANTENNAS(ANTENNAS)
    ) memory_bank (
        .clk(clk),
        .rst(rst),
        .load_channel_state(reading_channel_state),
        .load_symbols(reading_symbols),
        .set(set),
        .addr(addr),
        .data_in(data_in),
        .read_row_CICS_1(row_CICS_1),
        .read_col_CICS_1(col_CICS_1),
        .read_row_GBCD_1(row_GBCD_1),
        .read_col_GBCD_1(col_GBCD_1),
        .index_s_CICS_1(col_CICS_1),
        .index_s_GBCD_1(col_GBCD_1),
        .read_row(row),
        .read_col(col),
        .H_out(H_out_1),
        .Ginv_out_CICS_1(Ginv_out_CICS_1),
        .S_out_CICS_1(S_out_CICS_1),
        .Ginv_out_GBCD_1(Ginv_out_GBCD_1),
        .S_out_GBCD_1(S_out_GBCD_1),
        .read_row_CICS_2(row_CICS_2),
        .read_col_CICS_2(col_CICS_2),
        .read_row_GBCD_2(row_GBCD_2),
        .read_col_GBCD_2(col_GBCD_2),
        .index_s_CICS_2(col_CICS_2),
        .index_s_GBCD_2(col_GBCD_2),
        .Ginv_out_CICS_2(Ginv_out_CICS_2),
        .S_out_CICS_2(S_out_CICS_2),
        .Ginv_out_GBCD_2(Ginv_out_GBCD_2),
        .S_out_GBCD_2(S_out_GBCD_2),
        .CICS_1_selector(i_selector_CICS_1),
        .GBCD_1_selector(i_selector_GBCD_1),
        .CICS_2_selector(i_selector_CICS_2),
        .GBCD_2_selector(i_selector_GBCD_2),
        .g_mem_output_0(g_mem_output_0),
        .g_mem_output_1(g_mem_output_1),
        .g_mem_output_2(g_mem_output_2),
        .g_mem_output_3(g_mem_output_3),
        .chance(chance),
        .clear_output(rst | reset_output_1 | reset_output_2),
        .output_calculate(output_selector & !CI_precoded),
	.tr_out(tr_out),
        .entire_column_CICS_1(entire_column_CICS_1),
        .entire_column_CICS_2(entire_column_CICS_2),
        .entire_column_GBCD_1(entire_column_GBCD_1),
        .entire_column_GBCD_2(entire_column_GBCD_2),
        .entire_symbols_CICS_1(entire_symbols_CICS_1),
        .entire_symbols_CICS_2(entire_symbols_CICS_2),
        .entire_symbols_GBCD_1(entire_symbols_GBCD_1),
        .entire_symbols_GBCD_2(entire_symbols_GBCD_2)
    );
    
    gradient_intialization #(
        .DATA_WIDTH(DATA_WIDTH),
        .SIZE(USERS)
    ) gradient_0 (
      .clk(clk),
      .rst(rst | (CI_precoded & !i_selector_GBCD_1 & GBCD_completed_1)),
      .enable(gradient_enable_1 & !i_selector_CICS_1),
      .g_inv_input(Ginv_out_CICS_1),
      .read_index_CICS(gradient_index_CICS_1),
      .read_index_GBCD(gradient_index_GBCD_1),
      .read_index(col),
      .s_input(S_out_CICS_1),
      .index(row_CICS_1),
      .g_mem_output_CICS(g_mem_output_0_CICS_1),
      .g_mem_output_GBCD(g_mem_output_0_GBCD_1),
      .g_mem_output(g_mem_output_0),
      .write((update_step_g_CICS_1 & !i_selector_CICS_1) | (update_step_g_GBCD_1 & !i_selector_GBCD_1)),
      .step_g_CICS(step_g_CICS_1),
      .step_g_GBCD(step_g_GBCD_1),
      .is_CICS(update_step_g_CICS_1 & !i_selector_CICS_1),
      .updated(g_written_0),
      .reset_values((reset_values_CICS_1 & !i_selector_CICS_1) | (reset_values_GBCD_1 & !i_selector_GBCD_1)),
      .entire_column(entire_column_CICS_1)
    );
    
    gradient_intialization #(
        .DATA_WIDTH(DATA_WIDTH),
        .SIZE(USERS)
    ) gradient_1 (
      .clk(clk),
      .rst(rst | (CI_precoded & i_selector_GBCD_1 & GBCD_completed_1)),
      .enable(gradient_enable_1 & i_selector_CICS_1),
      .g_inv_input(Ginv_out_CICS_1),
      .read_index_CICS(gradient_index_CICS_1),
      .read_index_GBCD(gradient_index_GBCD_1),
      .read_index(col),
      .s_input(S_out_CICS_1),
      .index(row_CICS_1),
      .g_mem_output_CICS(g_mem_output_1_CICS_1),
      .g_mem_output_GBCD(g_mem_output_1_GBCD_1),
      .g_mem_output(g_mem_output_1),
      .write((update_step_g_CICS_1 & i_selector_CICS_1) | (update_step_g_GBCD_1 & i_selector_GBCD_1)),
      .step_g_CICS(step_g_CICS_1),
      .step_g_GBCD(step_g_GBCD_1),
      .is_CICS(update_step_g_CICS_1 & i_selector_CICS_1),
      .updated(g_written_1),
      .reset_values((reset_values_CICS_1 & i_selector_CICS_1) | (reset_values_GBCD_1 & i_selector_GBCD_1)),
      .entire_column(entire_column_CICS_1)
    );
     
     CICS #
    (
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS),
        .ANTENNAS(ANTENNAS)
    )
    CICS_UNIT_2
    (
        .clk(clk),
        .rst(rst),
        .modulation(modulation),
        .start_calculation(crossed_1 & trigger & more_outputs),
        .nmax(nmax),
        .row(row_CICS_2),
        .col(col_CICS_2),
        .S_out(S_out_CICS_2),
        .Ginv_out(Ginv_out_CICS_2),
        .g_mem_output_0(g_mem_output_0_CICS_2),
        .g_mem_output_1(g_mem_output_1_CICS_2),
        .g_written_0(g_written_2),
        .g_written_1(g_written_3),
        .gradient_enable(gradient_enable_2),
        .gradient_index(gradient_index_CICS_2),
        .step_g(step_g_CICS_2),
        .update_step_g(update_step_g_CICS_2),
        .completed(CICS_completed_2),
        .reset_values(reset_values_CICS_2),
        .i_selector(i_selector_CICS_2),
        .GBCD_not_busy(not_busy_GBCD_2),
        .waiting(CICS_waiting_2),
        .can_trigger(can_trigger_2),
        .crossed(crossed_2),
        .entire_column(entire_column_CICS_2),
        .entire_symbols(entire_symbols_CICS_2)
    );


    GBCD #
    (
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS),
        .ANTENNAS(ANTENNAS)
    )
    GBCD_UNIT_2
    (
        .clk(clk),
        .rst(rst),
        .modulation(modulation),
        .start_calculation(CICS_completed_2),
        .mmax(mmax),
        .row(row_GBCD_2),
        .col(col_GBCD_2),
        .S_out(S_out_GBCD_2),
        .Ginv_out(Ginv_out_GBCD_2),
        .g_mem_output_0(g_mem_output_0_GBCD_2),
        .g_mem_output_1(g_mem_output_1_GBCD_2),
        .g_written_0(g_written_2),
        .g_written_1(g_written_3),
        .gradient_index(gradient_index_GBCD_2),
        .step_g(step_g_GBCD_2),
        .update_step_g(update_step_g_GBCD_2),
        .completed(GBCD_completed_2),
        .reset_values(reset_values_GBCD_2),
        .i_selector(i_selector_GBCD_2),
        .chance(chance),
        .precoded(CI_precoded),
        .not_busy(not_busy_GBCD_2),
        .track(chance_2),
        .reset_output(reset_output_2),
        .entire_column(entire_column_GBCD_2),
        .entire_symbols(entire_symbols_GBCD_2)
    );
    
    gradient_intialization #(
        .DATA_WIDTH(DATA_WIDTH),
        .SIZE(USERS)
    ) gradient_2 (
      .clk(clk),
      .rst(rst | (CI_precoded & !i_selector_GBCD_2 & GBCD_completed_2)),
      .enable(gradient_enable_2 & !i_selector_CICS_2),
      .g_inv_input(Ginv_out_CICS_2),
      .read_index_CICS(gradient_index_CICS_2),
      .read_index_GBCD(gradient_index_GBCD_2),
      .read_index(col),
      .s_input(S_out_CICS_2),
      .index(row_CICS_2),
      .g_mem_output_CICS(g_mem_output_0_CICS_2),
      .g_mem_output_GBCD(g_mem_output_0_GBCD_2),
      .g_mem_output(g_mem_output_2),
      .write((update_step_g_CICS_2 & !i_selector_CICS_2) | (update_step_g_GBCD_2 & !i_selector_GBCD_2)),
      .step_g_CICS(step_g_CICS_2),
      .step_g_GBCD(step_g_GBCD_2),
      .is_CICS((update_step_g_CICS_2 & !i_selector_CICS_2)),
      .updated(g_written_2),
      .reset_values((reset_values_CICS_2 & !i_selector_CICS_2) | (reset_values_GBCD_2 & !i_selector_GBCD_2)),
      .entire_column(entire_column_CICS_2)
    );
    
    gradient_intialization #(
        .DATA_WIDTH(DATA_WIDTH),
        .SIZE(USERS)
    ) gradient_3 (
      .clk(clk),
      .rst(rst | (CI_precoded & i_selector_GBCD_2 & GBCD_completed_2)),
      .enable(gradient_enable_2 & i_selector_CICS_2),
      .g_inv_input(Ginv_out_CICS_2),
      .read_index_CICS(gradient_index_CICS_2),
      .read_index_GBCD(gradient_index_GBCD_2),
      .read_index(col),
      .s_input(S_out_CICS_2),
      .index(row_CICS_2),
      .g_mem_output_CICS(g_mem_output_1_CICS_2),
      .g_mem_output_GBCD(g_mem_output_1_GBCD_2),
      .g_mem_output(g_mem_output_3),
      .write((update_step_g_CICS_2 & i_selector_CICS_2) | (update_step_g_GBCD_2 & i_selector_GBCD_2)),
      .step_g_CICS(step_g_CICS_2),
      .step_g_GBCD(step_g_GBCD_2),
      .is_CICS((update_step_g_CICS_2 & i_selector_CICS_2)),
      .updated(g_written_3),
      .reset_values((reset_values_CICS_2 & i_selector_CICS_2) | (reset_values_GBCD_2 & i_selector_GBCD_2)),
      .entire_column(entire_column_CICS_2)
    );
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            row <= 0;
            col <= 0;
            CI_precoded <= 0;
            start_calculation_2 <= 0;
        end
        else if(output_selector && !CI_precoded)
        begin
            if(col < USERS - 1)
            begin
                col <= col + 1;
            end
            else
            begin
                col <= 0;
                CI_precoded <= 1;
            end
        end
        if(reset_output_1 | reset_output_2)
        begin
            CI_precoded <= 0;
        end
    end
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            firstTime <= 1;
        end
        else if(CICS_completed_1)
        begin
            firstTime <= 0;
        end
    end 
    
    always @(posedge CICS_completed_1 or posedge rst)
    begin
        if (rst)
            no_output1 <= 0;
        else
            no_output1 <= no_output1 + 1;
    end

    
    always @(posedge CICS_completed_2 or posedge rst)
        begin
            if (rst)
                no_output2 <= 0;
            else
                no_output2 <= no_output2 + 1;
        end
    
    
endmodule


module min_value_finder #(
    parameter DATA_WIDTH = 16,
    parameter USERS = 2*4
    )
    (
        input [USERS-1:0] A,
        input [USERS-1:0] B,
        input find_min_value,
        input clk,
        input rst,
        input [DATA_WIDTH-1:0] index,
        input [DATA_WIDTH-1:0] g_value,
        output [USERS-1:0] updated_A,
        output reg [DATA_WIDTH-1:0] minValue,
        output reg [DATA_WIDTH-1:0] min_index
    );
    
    integer A_set_index;
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            min_index = {DATA_WIDTH{1'b0}};
            minValue = {1'b0, {DATA_WIDTH-1{1'b1}}};  
        end
        else if(find_min_value)
        begin
            if(index < USERS)
            begin
                if(g_value[DATA_WIDTH-1] && g_value >= minValue && !B[index])
                begin
                    minValue = g_value;
                    min_index = index;
                end
                else if(!g_value[DATA_WIDTH-1] && g_value <= minValue && !B[index] && !minValue[DATA_WIDTH-1])
                begin
                    minValue = g_value;
                    min_index = index;
                end
             end
        end
    end
    
    assign updated_A = A | (1 << min_index);
    
endmodule

module input_storage #(
    parameter DATA_WIDTH = 16,
    parameter MATRIX_SIZE = 8,
    parameter USERS = 2 * 4,
    parameter ANTENNAS = 2 * 4
    )(
        input clk,
        input rst,
        input load_channel_state,
        input load_symbols,
        input [1:0] set,
        input [DATA_WIDTH-1:0] data_in,
        input [DATA_WIDTH-1:0] addr,
        input [DATA_WIDTH-1:0] read_row,
        input[DATA_WIDTH-1:0] read_col,
        input [DATA_WIDTH-1:0] read_row_CICS_1,
        input[DATA_WIDTH-1:0] read_col_CICS_1,
        input [DATA_WIDTH-1:0] index_s_CICS_1,
        input [DATA_WIDTH-1:0] read_row_GBCD_1,
        input[DATA_WIDTH-1:0] read_col_GBCD_1,
        input [DATA_WIDTH-1:0] index_s_GBCD_1,
        output [DATA_WIDTH-1:0] H_out,
        output [DATA_WIDTH-1:0] Ginv_out_CICS_1,
        output [DATA_WIDTH-1:0] S_out_CICS_1,
        output [DATA_WIDTH-1:0] Ginv_out_GBCD_1,
        output [DATA_WIDTH-1:0] S_out_GBCD_1,
        input [DATA_WIDTH-1:0] read_row_CICS_2,
        input[DATA_WIDTH-1:0] read_col_CICS_2,
        input [DATA_WIDTH-1:0] index_s_CICS_2,
        input [DATA_WIDTH-1:0] read_row_GBCD_2,
        input[DATA_WIDTH-1:0] read_col_GBCD_2,
        input [DATA_WIDTH-1:0] index_s_GBCD_2,
        output [DATA_WIDTH-1:0] Ginv_out_CICS_2,
        output [DATA_WIDTH-1:0] S_out_CICS_2,
        output [DATA_WIDTH-1:0] Ginv_out_GBCD_2,
        output [DATA_WIDTH-1:0] S_out_GBCD_2,
        input CICS_1_selector,
        input CICS_2_selector,
        input GBCD_1_selector,
        input GBCD_2_selector,
        input [DATA_WIDTH-1:0] g_mem_output_0,
        input [DATA_WIDTH-1:0] g_mem_output_1,
        input [DATA_WIDTH-1:0] g_mem_output_2,
        input [DATA_WIDTH-1:0] g_mem_output_3,
        input chance,
        input output_calculate,
        input clear_output,
	output [ANTENNAS*DATA_WIDTH-1:0] tr_out,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_column_CICS_1,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_column_CICS_2,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_column_GBCD_1,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_column_GBCD_2,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_symbols_CICS_1,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_symbols_CICS_2,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_symbols_GBCD_1,
        output [DATA_WIDTH*MATRIX_SIZE-1:0] entire_symbols_GBCD_2
    );
    
    localparam END_INDEX = (DATA_WIDTH-1) + ((DATA_WIDTH-1)/2);
    localparam START_INDEX = (DATA_WIDTH-1) - ((DATA_WIDTH-1)/2) ;
    localparam HALF = DATA_WIDTH / 2;
    
    reg [DATA_WIDTH-1:0] H_mem [0:ANTENNAS-1][0:USERS-1];
    reg [DATA_WIDTH-1:0] Ginv_mem [0:MATRIX_SIZE-1][0:MATRIX_SIZE-1];
    
    reg [DATA_WIDTH-1:0] S0_mem [0:MATRIX_SIZE-1]; // BLOCK_1 first
    reg [DATA_WIDTH-1:0] S1_mem [0:MATRIX_SIZE-1]; // BLOCK_1 third
    reg [DATA_WIDTH-1:0] S2_mem [0:MATRIX_SIZE-1]; // BLOCK_2 second
    reg [DATA_WIDTH-1:0] S3_mem [0:MATRIX_SIZE-1]; // BLOCK_2 fourth
    
    integer i,j,i1,j1;
    
    always @(posedge clk or posedge rst)
    begin
        if(rst)
        begin
            for (i1 = 0; i1 < ANTENNAS; i1 = i1 + 1) begin
                S0_mem[i1] <= 0;
                S1_mem[i1] <= 0;
                S2_mem[i1] <= 0;
                S3_mem[i1] <= 0;
                for (j1 = 0; j1 < ANTENNAS; j1 = j1 + 1) begin
                    H_mem[i1][j1] <= 0;
                    Ginv_mem[i1][j1] <= 0;
                end
            end
            i <= 0;
            j <= 0;
        end
        else if(load_channel_state)
        begin
            if(addr < USERS * ANTENNAS)
            begin
                H_mem[addr / USERS][addr % USERS] <= data_in;
            end
            else if (addr < USERS * ANTENNAS + MATRIX_SIZE * MATRIX_SIZE) begin
                Ginv_mem[i][j] <= data_in;
                if(j == MATRIX_SIZE - 1)
                begin
                    j <= 0;
                    if(i == MATRIX_SIZE - 1)
                    begin
                        i <= 0;
                    end
                    else
                    begin
                        i <= i + 1;
                    end
                end
                else 
                begin
                    j <= j + 1;
                end
           end
        end
        else if(load_symbols)
        begin
            if(set == 0)
            begin
                S0_mem[i] <= data_in;
                if(i == USERS - 1)
                begin
                    i <= 0;
                end
                else begin
                    i <= i + 1;
                end
            end
            else if(set == 2)
            begin
                S1_mem[i] <= data_in;
                if(i == USERS - 1)
                begin
                    i <= 0;
                end
                else begin
                    i <= i + 1;
                end
            end
            else if(set == 1)
            begin
                S2_mem[i] <= data_in;
                if(i == USERS - 1)
                begin
                    i <= 0;
                end
                else begin
                    i <= i + 1;
                end
            end
            else if(set == 3)
            begin
                S3_mem[i] <= data_in;
                if(i == USERS - 1)
                begin
                    i <= 0;
                end
                else begin
                    i <= i + 1;
                end
            end
        end
    end

        assign H_out = H_mem[read_row][read_col];
        
        assign Ginv_out_CICS_1 = Ginv_mem[read_row_CICS_1][read_col_CICS_1];
        assign S_out_CICS_1 = CICS_1_selector ? S1_mem[index_s_CICS_1] : S0_mem[index_s_CICS_1];
        assign Ginv_out_GBCD_1 = Ginv_mem[read_row_GBCD_1][read_col_GBCD_1];
        assign S_out_GBCD_1 = GBCD_1_selector ? S1_mem[index_s_GBCD_1] : S0_mem[index_s_GBCD_1];
        
        assign Ginv_out_CICS_2 = Ginv_mem[read_row_CICS_2][read_col_CICS_2];
        assign S_out_CICS_2 = CICS_2_selector ? S3_mem[index_s_CICS_2] : S2_mem[index_s_CICS_2];
        assign Ginv_out_GBCD_2 = Ginv_mem[read_row_GBCD_2][read_col_GBCD_2];
        assign S_out_GBCD_2 = GBCD_2_selector ? S3_mem[index_s_GBCD_2] : S2_mem[index_s_GBCD_2];
    
    reg [DATA_WIDTH-1:0] x_real_valued [0:ANTENNAS-1];
    reg [DATA_WIDTH-1:0] newT [0:USERS-1];
    wire [DATA_WIDTH-1:0] firstTerm [0:USERS-1];
    wire [2*(DATA_WIDTH-1)-1:0] rp [0:USERS-1];
    reg sign [0:USERS-1];
    
    wire [DATA_WIDTH-1:0] g_mem_output;
    
    assign g_mem_output = output_calculate ? chance ? GBCD_2_selector ? g_mem_output_3 : g_mem_output_2 : GBCD_1_selector ? g_mem_output_1 : g_mem_output_0 : {DATA_WIDTH{1'b0}} ;
    
    integer index;
    
    genvar e;
    
    generate
        for(e=0;e<ANTENNAS;e=e+1) begin : output_values
            assign firstTerm[e] = H_mem[e][read_col];
            assign rp[e] = firstTerm[e][DATA_WIDTH-2:0] * g_mem_output[DATA_WIDTH-2:0];
            
            always @(posedge clk)
            begin
                if(clear_output) begin
                    x_real_valued[e] = 0;
                end
                else if(output_calculate) begin
                    newT[e] = {firstTerm[e][DATA_WIDTH-1] ^ g_mem_output[DATA_WIDTH-1], rp[e][END_INDEX : START_INDEX]};
                    sign[e] = x_real_valued[e][DATA_WIDTH-1] ^ newT[e][DATA_WIDTH-1];
                    if(sign[e] == 0)
                    begin
                      x_real_valued[e] = {x_real_valued[e][DATA_WIDTH-1],x_real_valued[e][DATA_WIDTH-2:0] + newT[e][DATA_WIDTH-2:0]};
                    end
                    else
                    begin                 
                        if(x_real_valued[e][DATA_WIDTH-2:0] >= newT[e][DATA_WIDTH-2:0])
                        begin
                           x_real_valued[e] = {x_real_valued[e][DATA_WIDTH-1],x_real_valued[e][DATA_WIDTH-2:0] - newT[e][DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                            x_real_valued[e] = {newT[e][DATA_WIDTH-1],newT[e][DATA_WIDTH-2:0] - x_real_valued[e][DATA_WIDTH-2:0]};
                        end 
                    end
                    if(read_col == USERS - 1)
                    begin
                        x_real_valued[e][DATA_WIDTH-1] = x_real_valued[e][DATA_WIDTH-1] ^ 1;
                    end
                end
            end
        end
    endgenerate

	assign tr_out = {x_real_valued[0],x_real_valued[1],x_real_valued[2],x_real_valued[3],x_real_valued[4],x_real_valued[5],x_real_valued[6],x_real_valued[7],
			x_real_valued[8],x_real_valued[9],x_real_valued[10],x_real_valued[11],x_real_valued[12],x_real_valued[13],x_real_valued[14],x_real_valued[15],
			x_real_valued[16],x_real_valued[17],x_real_valued[18],x_real_valued[19],x_real_valued[20],x_real_valued[21],x_real_valued[22],x_real_valued[23],
			x_real_valued[24],x_real_valued[25],x_real_valued[26],x_real_valued[27],x_real_valued[28],x_real_valued[29],x_real_valued[30],x_real_valued[31]			
			};

    genvar f;
    generate
        for(f=0;f<MATRIX_SIZE;f=f+1) begin : load_entire_col
            assign entire_column_CICS_1[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = Ginv_mem[f][read_col_CICS_1];
            assign entire_column_CICS_2[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = Ginv_mem[f][read_col_CICS_2];
            assign entire_column_GBCD_1[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = Ginv_mem[f][read_col_GBCD_1];
            assign entire_column_GBCD_2[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = Ginv_mem[f][read_col_GBCD_2];
            assign entire_symbols_CICS_1[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = CICS_1_selector ? S1_mem[f] : S0_mem[f];
            assign entire_symbols_CICS_2[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = CICS_2_selector ? S3_mem[f] : S2_mem[f];
            assign entire_symbols_GBCD_1[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = GBCD_1_selector ? S1_mem[f] : S0_mem[f];
            assign entire_symbols_GBCD_2[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] = GBCD_2_selector ? S3_mem[f] : S2_mem[f];
        end
    endgenerate
    
endmodule

module gradient_intialization #(
   parameter DATA_WIDTH = 16,
   parameter SIZE = 8
    )(
    input clk,
    input rst,
    input enable,
    input [DATA_WIDTH-1:0] g_inv_input,
    input [DATA_WIDTH-1:0] s_input,
    input [DATA_WIDTH-1:0] index,
    input [DATA_WIDTH-1:0] read_index_CICS,
    input [DATA_WIDTH-1:0] read_index_GBCD,
    input [DATA_WIDTH-1:0] read_index,
    output [DATA_WIDTH-1:0] g_mem_output,
    output [DATA_WIDTH-1:0] g_mem_output_CICS,
    output [DATA_WIDTH-1:0] g_mem_output_GBCD,
    input [DATA_WIDTH*SIZE-1:0] step_g_CICS,
    input [DATA_WIDTH*SIZE-1:0] step_g_GBCD,
    input write,
    input reset_values,
    output reg updated,
    input is_CICS,
    input [DATA_WIDTH*SIZE-1:0] entire_column
    );
    
    wire [DATA_WIDTH*SIZE-1:0] step_g;
    assign step_g = is_CICS ? step_g_CICS : step_g_GBCD;
    
    
    localparam END_INDEX = (DATA_WIDTH-1) + ((DATA_WIDTH-1)/2);
    localparam START_INDEX = (DATA_WIDTH-1) - ((DATA_WIDTH-1)/2);
    
    wire [DATA_WIDTH-1:0] S_input;

    
    assign S_input = enable ? s_input : {DATA_WIDTH{1'b0}};
    
    reg [DATA_WIDTH-1:0] step_g_vector [0:SIZE-1];
    
    reg update_values;
    
    genvar e;
    
    generate
        for (e = 0; e < SIZE; e = e + 1) begin : step_g_unpack
            always @ (*) begin
                    step_g_vector[e] = step_g[(e+1)*DATA_WIDTH-1 : e*DATA_WIDTH];
                    if(step_g_vector[e] == {1'b1, {DATA_WIDTH-1{1'b0}}})
                    begin
                        step_g_vector[e] = {DATA_WIDTH{1'b0}};
                    end
               end
        end   
    endgenerate

    
    reg [DATA_WIDTH-1:0] G_memory [0:SIZE-1];
    wire [DATA_WIDTH-1:0] Ginv_memory [0:SIZE-1];
    
    reg [DATA_WIDTH-1:0] NewTerm [0:SIZE-1];   
    reg Sign [0:SIZE-1];
    wire [2*(DATA_WIDTH-1)-1:0] Product [0:SIZE-1];
    reg [DATA_WIDTH-1:0] Result [0:SIZE-1];
    integer Count [0:SIZE-1];
    reg UpdSign [0:SIZE-1];
    reg wait_once [0:SIZE-1];
    
    genvar f,g;
    
    generate 
    for(g=0;g<SIZE;g=g+1) begin: extract_from_column
        assign Ginv_memory[g] = entire_column[(g+1)*DATA_WIDTH-1:g*DATA_WIDTH];
    end
    endgenerate
    
    generate
    for(f=0;f<SIZE;f=f+1) begin: gradient_manage
        assign Product[f] =  Ginv_memory[f][DATA_WIDTH-2:0] * S_input[DATA_WIDTH-2:0];
        always @(posedge clk)
        begin
            if(rst)
            begin
                G_memory[f] = 0;
                Result[f] = 0;
                Count[f] = 0;
                wait_once[f] <= 1;
                NewTerm[f] <= 0;
                Sign[f] = 0;
                if(f == SIZE-1) updated <= 0;
            end
            else if(enable) 
            begin
            
                NewTerm[f] <= {Ginv_memory[f][DATA_WIDTH-1]^S_input[DATA_WIDTH-1],Product[f][END_INDEX:START_INDEX]};
                
                if(wait_once[f]) wait_once[f] <= 0;
                else begin
                Sign[f] = Result[f][DATA_WIDTH-1]^NewTerm[f][DATA_WIDTH-1];
                if(!Sign[f])
                begin
                    Result[f] = {Result[f][DATA_WIDTH-1],Result[f][DATA_WIDTH-2:0] + NewTerm[f][DATA_WIDTH-2:0]};
                end
                else
                begin
                    if(Result[f][DATA_WIDTH-2:0] >= NewTerm[f][DATA_WIDTH-2:0])
                    begin
                        Result[f] = {Result[f][DATA_WIDTH-1],Result[f][DATA_WIDTH-2:0] - NewTerm[f][DATA_WIDTH-2:0]};
                    end
                    else
                    begin
                        Result[f] = {NewTerm[f][DATA_WIDTH-1],NewTerm[f][DATA_WIDTH-2:0] - Result[f][DATA_WIDTH-2:0]};
                    end
                end
                Count[f] = Count[f] + 1;
                if(Count[f] == SIZE)
                begin
                    G_memory[f] = {1'b1 ^ Result[f][DATA_WIDTH-1],Result[f][DATA_WIDTH-2:0]};
                end
                end
            end
            else if(write)
            begin
                UpdSign[f] = G_memory[f][DATA_WIDTH-1] ^ step_g_vector[f][DATA_WIDTH-1];
                if(!UpdSign[f])
                begin
                    G_memory[f] = {G_memory[f][DATA_WIDTH-1],G_memory[f][DATA_WIDTH-2:0] + step_g_vector[f][DATA_WIDTH-2:0]};
                end
                else
                begin
                    if(G_memory[f][DATA_WIDTH-2:0] >= step_g_vector[f][DATA_WIDTH-2:0])
                    begin
                        G_memory[f] = {G_memory[f][DATA_WIDTH-1],G_memory[f][DATA_WIDTH-2:0] - step_g_vector[f][DATA_WIDTH-2:0]};
                    end
                    else                            
                    begin
                        G_memory[f] = {step_g_vector[f][DATA_WIDTH-1],step_g_vector[f][DATA_WIDTH-2:0] - G_memory[f][DATA_WIDTH-2:0]};
                    end
                end
            end
        end
    end
    endgenerate
    
    assign g_mem_output = G_memory[read_index];
    assign g_mem_output_CICS = G_memory[read_index_CICS];
    assign g_mem_output_GBCD = G_memory[read_index_GBCD];


endmodule

module GBCD #(
    parameter DATA_WIDTH = 16,
    parameter USERS = 2 * 4,
    parameter ANTENNAS = 2 * 4
    )(
        input clk,
        input rst,
        input start_calculation,
        input [31:0] modulation,
        input [2:0] mmax,
        output reg [DATA_WIDTH-1:0] row,
        output reg [DATA_WIDTH-1:0] col,
        input [DATA_WIDTH-1:0] S_out,
        input [DATA_WIDTH-1:0] Ginv_out,
        input [DATA_WIDTH-1:0] g_mem_output_0,
        input [DATA_WIDTH-1:0] g_mem_output_1,
        input g_written_0,
        input g_written_1,
        output reg [DATA_WIDTH-1:0] gradient_index,
        output [DATA_WIDTH*USERS-1:0] step_g,
        output update_step_g,
        output completed,
        output reset_values,
        output reg i_selector,
        input chance,
        input precoded,
        output not_busy,
        output reg track, 
        output reg reset_output,
        input [DATA_WIDTH*USERS-1:0] entire_column,
        input [DATA_WIDTH*USERS-1:0] entire_symbols
    );
    
    wire g_written;
    assign g_written = i_selector ? g_written_1 : g_written_0;
    wire [DATA_WIDTH-1:0] g_mem_output;
    assign g_mem_output = i_selector ? g_mem_output_1 : g_mem_output_0;
    
    localparam HALF = DATA_WIDTH / 2;
    integer outer;

    reg [USERS-1:0] A,B;

    reg [3:0] state,iteration;

    
    wire find_value,step_z_calculate,calculate_step_g,step_g_extracted;

    wire [DATA_WIDTH-1:0] minIndex,secondMinIndex,minValue,secondMinValue;
    wire [DATA_WIDTH-1:0] smallerIndex,largerIndex;
    
    wire [DATA_WIDTH-1:0] g_value;
    reg satisfy;
    
    wire finished_step_z;
    reg [2:0] index;
    reg check;
    
    reg [DATA_WIDTH*USERS-1:0] step_z;
    wire [DATA_WIDTH-1:0] z1,z2,result,division_result;
    
    reg [1:0] operation;
    reg [DATA_WIDTH-1:0] a,b;
    reg [DATA_WIDTH-1:0] size;
    
    reg reset;
    
    find_min_second_value #(
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS)
    ) find_min_coordinates
    (
        .B(B),
        .find_value(find_value),
        .clk(clk),
        .rst(rst | reset_values | reset),
        .index(gradient_index),
        .g_value(g_value),
        .minValue(minValue),
        .minIndex(minIndex),
        .secondMinValue(secondMinValue),
        .secondMinIndex(secondMinIndex)
    );
    
    extract_step_z_gbcd #(
        .DATA_WIDTH(DATA_WIDTH)
    ) step_z_calculator (
        .clk(clk),
        .rst(rst | reset_values | reset),
        .enable(step_z_calculate),
        .ginv_mem(Ginv_out),
        .g_mem(g_mem_output),
        .index(index),
        .finished(finished_step_z),
        .z1(z1),
        .z2(z2)
    );
        
   wire div_enable, divided;
   assign div_enable = operation == 3;
   
   fixed_division #(.WIDTH(DATA_WIDTH)) 
        divisioner (
            .clk(clk),
            .enable(div_enable),
            .rst(rst | reset_values),
            .a(a),
            .b(b),
            .result(division_result),
            .finished(divided)
        );
        
    extract_stepG #(
        .DATA_WIDTH(DATA_WIDTH),
        .MATRIX_SIZE(USERS)
    ) stepg_calculator
    (
        .A_set(A),
        .size(size),
        .clk(clk),
        .rst(rst | reset_values | reset),
        .ginv_mem(Ginv_out),
        .row(row),
        .col(col),
        .enable(calculate_step_g),
        .step_za(step_z),
        .step_g(step_g),
        .done(step_g_extracted),
        .entire_column(entire_column)
    );

    assign find_value = state == 2;
    assign step_z_calculate = state == 3 | state == 4;
    assign calculate_step_g = state == 9 | state == 10;
    assign update_step_g = state == 11;
    assign reset_values = state == 12;
    assign completed = state == 14 & !reset_output;
    assign not_busy = state == 15;
    
    always @(*)
    begin
        outer = modulation == 16 ? 3 : modulation == 64 ? 7 : 1;
    end
    
    assign g_value = {S_out[DATA_WIDTH-1] ^ g_mem_output[DATA_WIDTH-1], g_mem_output[DATA_WIDTH-2:0]};

    assign smallerIndex = minIndex < secondMinIndex ? minIndex : secondMinIndex;
    assign largerIndex = minIndex < secondMinIndex ? secondMinIndex : minIndex;
    
    wire [DATA_WIDTH-1:0] SOUT [0:USERS-1];
    
    genvar e;
    generate
        for(e=0;e<USERS;e=e+1) begin : B_calculate
            assign SOUT[e] = entire_symbols[(e+1)*DATA_WIDTH-1:e*DATA_WIDTH];
            always @(rst or state)
            begin
                if(rst)  
                begin
                    B[e] <= 0;
                end
                else
                begin
                    case (state)
                        0:
                        begin
                            if(SOUT[e][DATA_WIDTH-2:HALF] < outer)
                            begin
                                B[e] <= 1;
                            end
                        end
                        13:
                        begin
                            B[e] <= 0;
                        end
                    endcase
                end
            end
        end
    endgenerate
    
    reg c;
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            state <= 15;
            col <= {DATA_WIDTH{1'b0}};
            row <= {DATA_WIDTH{1'b0}};
            A <= {USERS{1'b0}};
            gradient_index <= {DATA_WIDTH{1'b0}};
            satisfy <= 1;
            iteration <= 0;
            index <= 0;
            step_z <= 0;
            check <= 0;
            operation <= 0;
            a <= 0;
            b <= 0;
            size <= {DATA_WIDTH{1'b0}};
            i_selector <= 0;
            c <= 0;
            track <= 0;
            reset_output <= 0;
            reset <= 0;
        end
        else
        begin
            case (state)
            0:
            begin
                state <= 2;
            end
            2:
            begin
                if(gradient_index < USERS) 
                begin
                    gradient_index <= gradient_index + 1;
                    col <= col + 1;
                end
                else
                begin
                    row <= minIndex;
                    col <= minIndex;
                    if((minValue[DATA_WIDTH-1] == 0) | (minValue[DATA_WIDTH-2:0] == 0))
                    begin
                        gradient_index <= {DATA_WIDTH{1'b0}};
                        state <= 13;
                        reset <= 1;
                        iteration <= mmax;
                    end
                    else
                    begin
                        state <= 3;
                    end
                end
            end
            3:
            begin
                if(index == 0)
                begin
                    col <= secondMinIndex;
                    index <= index + 1;
                end
                else if(index == 1)
                begin
                    row <= secondMinIndex;
                    col <= minIndex;
                    index <= index + 1;
                end
                else if(index == 2)
                begin
                    col <= secondMinIndex;
                    index <= index + 1;
                end
                else if(index == 3)
                begin
                    gradient_index <= minIndex;
                    index <= index + 1;
                end
                else if(index == 4)
                begin
                    gradient_index <= secondMinIndex;
                    index <= index + 1;
                end
                else if(index == 5)
                begin
                    state <= 4;
                end
            end
            4:
            begin
                if(finished_step_z)
                begin
                    state <= 5;
                    col <= minIndex;
                end
            end
            5:
            begin
                if(check == 0)
                begin
                    if(S_out[DATA_WIDTH-1] != z1[DATA_WIDTH-1])
                    begin
                        satisfy <= 0;
                    end
                    check <= 1;
                    col <= secondMinIndex;
                end
                else if(check)
                begin
                    if(S_out[DATA_WIDTH-1] != z2[DATA_WIDTH-1])
                    begin
                        satisfy <= 0;
                    end
                    state <= 6;
                    check <= 0;
                end
            end
            6:
            begin
                if(!satisfy)
                begin
                    col <= minIndex;
                    row <= minIndex;
                    gradient_index <= minIndex;
                    A <= (1 << minIndex);
                    size <= 1;
                    state <= 7;
                end
                else
                begin
                    step_z[DATA_WIDTH-1:0] <= z1;
                    step_z[2*DATA_WIDTH-1:DATA_WIDTH] <= z2;
                    A <= (1 << minIndex) | (1 << secondMinIndex);
                    size <= 2;
                    state <= 9;
                    col <= smallerIndex;
                    row <= {DATA_WIDTH{1'b0}};
                end
            end
            7:
            begin
                a <= g_mem_output;
                b <= Ginv_out; 
                operation <= 3;
                if(divided)
                begin
                    state <= 8;
                end
            end
            8:
            begin
                step_z[DATA_WIDTH-1:0] <= division_result;
                step_z[2*DATA_WIDTH-1:DATA_WIDTH] <= 0;
                operation <= 0;
                state <= 9;
                col <= smallerIndex;
                row <= {DATA_WIDTH{1'b0}};
            end
            9:
            begin
                col <= largerIndex;
                state <= 10;
            end
            10:
            begin
                if(step_g_extracted)
                begin
                    state <= 11;
                end
            end
            11:
            begin
                state <= 12;
            end
            12:
            begin
                col <= {DATA_WIDTH{1'b0}};
                row <= {DATA_WIDTH{1'b0}};
                gradient_index <= {DATA_WIDTH{1'b0}};
                index <= 0;
                if(iteration < mmax)
                begin
                    satisfy <= 1;
                    state <= 2;
                    iteration <= iteration + 1;
                    // index <= 0;
                end
                else
                begin
                    state <= 13;
                end
            end
            13:
            begin
                if(reset)
                begin
                    reset <= 0;
                    iteration <= 0;
                end
                if(chance)
                begin
                    state <= 14;
                    reset_output <= 1;
                    A <= {USERS{1'b0}};
                    iteration <= 0;
                end
            end
            14:
            begin
                if(reset_output)
                begin
                    reset_output <= 0;
                end
                else if(precoded)
                begin
                    state <= 15;
                    i_selector <= i_selector + 1;
                    c <= 0;
                    track <= track + 1;
                end
            end
            15:
            begin
                if(start_calculation)
                begin
                    c <= 1;
                end
                if(c == 1)
                begin
                    state <= 0;
                end
            end
        endcase
        end
    end
    
endmodule

module fixed_division#(
    parameter WIDTH = 16    
)(
    input [WIDTH-1:0] a,
    input [WIDTH-1:0] b,
    output reg [WIDTH-1:0] result,
    input enable,
    input clk,
    input rst,
    output reg finished
);

    localparam END_INDEX = (WIDTH-1) + ((WIDTH-1)/2);
    localparam START_INDEX = (WIDTH-1) - ((WIDTH-1)/2) ;
    localparam HALF = WIDTH / 2;

    reg [4:0] step;
    
    wire sign;
    assign sign = a[WIDTH-1] ^ b[WIDTH-1];
    
    reg [2*(WIDTH-1)-1:0] rp, interProduct, product;
    
    reg [HALF-1:0] scaleFactor;
    reg shiftDir;
    
    integer i;
    reg scaleFound;
    reg anotherBit;
    
    wire [WIDTH-1:0] firstTerm;
    assign firstTerm = 16'h02D3;
    wire [WIDTH-1:0] coeff;
    assign coeff = 16'h01E1;
    
    wire [WIDTH-1:0] two;
    assign two = 16'h0200;
    
    reg [WIDTH-1:0] temp_b;
    reg [WIDTH-1:0] z0;
    reg [WIDTH-1:0] z1,z2,z3;
    reg [WIDTH-1:0] interSum;
    
    wire [2*(WIDTH-1)-1:0] interProduct1,interProduct2,interProduct3,interProduct4,interProduct5;
    assign interProduct1 = coeff * temp_b;
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            step <= 0;
            result <= 0;
            scaleFound = 0;
            scaleFactor = 0;
            anotherBit = 0;
            shiftDir = 0;
            finished <= 0;
        end
        else if(enable)
        begin
            if(step == 0)
            begin
                if(b[WIDTH-2:HALF])
                begin
                     shiftDir = 0;

                     //case ({1'b0,b[WIDTH-2:HALF]})  // Checking MSB half
                        /*8'b01??????: scaleFactor = 8'd6;
                        8'b001?????: scaleFactor = 8'd5;
                        8'b0001????: scaleFactor = 8'd4;
                        8'b00001???: scaleFactor = 8'd3;
                        8'b000001??: scaleFactor = 8'd2;
                        8'b0000001?: scaleFactor = 8'd1;
                        8'b00000001: scaleFactor = 8'd0;*/
                       // default: scaleFactor = 8'd0;
                    //endcase  

		    if(b[WIDTH-2]) scaleFactor = 8'd6;
		    else if(b[WIDTH-2-1]) scaleFactor = 8'd5;
	    	    else if(b[WIDTH-2-2]) scaleFactor = 8'd4;
	    	    else if(b[WIDTH-2-3]) scaleFactor = 8'd3;
	    	    else if(b[WIDTH-2-4]) scaleFactor = 8'd2;
	    	    else if(b[WIDTH-2-5]) scaleFactor = 8'd1;
	    	    else scaleFactor = 8'd0;
 
                    if( b[WIDTH-2:HALF] & ~(1<<scaleFactor) )
                    begin
                        anotherBit = 1;
                        scaleFactor = scaleFactor + 1;
                    end
                    if(b[HALF-1:0] > 0 && anotherBit == 0)
                    begin
                        scaleFactor = scaleFactor + 1;
                    end
                end
                else if(b[HALF-1:0] < (1<<(HALF-1)))
                begin
                    shiftDir = 1;

                    //case (b[HALF-1:0])  // Checking LSB half
                        /*8'b1???????: scaleFactor = 8'd0;
                        8'b01??????: scaleFactor = 8'd1;
                        8'b001?????: scaleFactor = 8'd2;
                        8'b0001????: scaleFactor = 8'd3;
                        8'b00001???: scaleFactor = 8'd4;
                        8'b000001??: scaleFactor = 8'd5;
                        8'b0000001?: scaleFactor = 8'd6;
                        8'b00000001: scaleFactor = 8'd7;*/
                  //  default: scaleFactor = 8'd0;
                //endcase

		    if(b[HALF-1]) scaleFactor = 8'd0;
		    else if(b[HALF-1-1]) scaleFactor = 8'd1;
		    else if(b[HALF-1-2]) scaleFactor = 8'd2;
		    else if(b[HALF-1-3]) scaleFactor = 8'd3;
		    else if(b[HALF-1-4]) scaleFactor = 8'd4;
		    else if(b[HALF-1-5]) scaleFactor = 8'd5;
		    else if(b[HALF-1-6]) scaleFactor = 8'd6;
		    else scaleFactor = 8'd7;

                end
                step <= 1;
            end
            else if(step == 1)
            begin
                temp_b = shiftDir ? {1'b0,b[WIDTH-2:0]} << scaleFactor : {1'b0,b[WIDTH-2:0]} >> scaleFactor;
                step <= 2;
            end
            else if(step == 2)
            begin
                z0 <= firstTerm - {1'b0,interProduct1[END_INDEX : START_INDEX]};
                step <= 3;
            end
            else if(step == 3)
            begin
                interProduct = z0 * temp_b;
                step <= 4;
            end
            else if(step == 4)
            begin
                interSum = two - {1'b0,interProduct[END_INDEX : START_INDEX]};
                product = z0 * interSum;
                step <= 5;
            end
            else if(step == 5)
            begin
                z1 <= {1'b0,product[END_INDEX:START_INDEX]};
                step <= 6;
            end
            else if(step == 6)
            begin
                interProduct = z1 * temp_b;
                step <= 7;
            end
            else if(step == 7)
            begin
                interSum = two - {1'b0,interProduct[END_INDEX : START_INDEX]};
                product = z1 * interSum;
                step <= 8;
            end
            else if(step == 8)
            begin
                z2 <= {1'b0,product[END_INDEX:START_INDEX]};
                step <= 9;
            end
            else if(step == 9)
            begin
                interProduct = z2 * temp_b;
                step <= 10;
            end
            else if(step == 10)
            begin
                interSum = two - {1'b0,interProduct[END_INDEX : START_INDEX]};
                product = z2 * interSum;
                step <= 11;
            end
            else if(step == 11)
            begin
                z3 <= {1'b0,product[END_INDEX:START_INDEX]};
                step <= 12;
            end
            else if(step == 12)
            begin
                interProduct = a[WIDTH-2:0] * z3;
                interProduct = shiftDir ? interProduct << scaleFactor : interProduct >> scaleFactor;
                step <= 13;
            end
            else if(step == 13)
            begin
                result <= {sign, interProduct[END_INDEX : START_INDEX]};
                finished <= 1;
                step <= 14;
            end
        end
    end

endmodule

module find_min_second_value #(
    parameter DATA_WIDTH = 16,
    parameter USERS = 2*4
    )(
        input [USERS-1:0] B,
        input find_value,
        input clk,
        input rst,
        input [DATA_WIDTH-1:0] index,
        input [DATA_WIDTH-1:0] g_value,
        output reg [DATA_WIDTH-1:0] minValue,
        output reg [DATA_WIDTH-1:0] secondMinValue,
        output reg [DATA_WIDTH-1:0] minIndex,
        output reg [DATA_WIDTH-1:0] secondMinIndex
    );
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            minIndex = {DATA_WIDTH{1'b0}};
            minValue = {1'b0, {DATA_WIDTH-1{1'b1}}};  
            secondMinIndex = {DATA_WIDTH{1'b0}};
            secondMinValue = {1'b0, {DATA_WIDTH-1{1'b1}}}; 
        end
        else if(find_value)
        begin
            if(index < USERS)
            begin
                if(g_value[DATA_WIDTH-1] && g_value >= minValue && !B[index])
                begin
                    secondMinValue = minValue;
                    minValue = g_value;
                    secondMinIndex = minIndex;
                    minIndex = index;
                end
                else if(!g_value[DATA_WIDTH-1] && g_value <= minValue && !B[index] && !minValue[DATA_WIDTH-1])
                begin
                    secondMinValue = minValue;
                    minValue = g_value;
                    secondMinIndex = minIndex;
                    minIndex = index;
                end
                else if(g_value[DATA_WIDTH-1] && g_value >= secondMinValue && !B[index])
                begin
                    secondMinValue = g_value;
                    secondMinIndex = index;
                end
                else if(!g_value[DATA_WIDTH-1] && g_value <= secondMinValue && !B[index] && !secondMinValue[DATA_WIDTH-1])
                begin
                    secondMinValue = g_value;
                    secondMinIndex = index;
                end
            end
        end
    end
    
endmodule

module extract_submatrix #(
    parameter DATA_WIDTH = 16,
    parameter MATRIX_SIZE = 2 * 4
)
(
    input [MATRIX_SIZE-1:0] A_set,
    input [DATA_WIDTH-1:0] ginv_mem,
    input [DATA_WIDTH-1:0] g_min,
    input [DATA_WIDTH-1:0] size,
    input [DATA_WIDTH-1:0] size_square,
    input enable,
    input rst,
    input [DATA_WIDTH-1:0] row,
    input [DATA_WIDTH-1:0] col,
    input clk,
    input [DATA_WIDTH-1:0] relative_index,
    output ready,
    output [MATRIX_SIZE * DATA_WIDTH -1:0] final_step_za
);

    reg [DATA_WIDTH-1:0] send_size,sub_index;
    
    integer i,j,i1,j1;
        
    reg [DATA_WIDTH-1:0] Ginv_A_A [0:MATRIX_SIZE-1][0:MATRIX_SIZE-1];
    
    reg [DATA_WIDTH*MATRIX_SIZE -1:0] delta;
    
    wire [DATA_WIDTH-1:0] epsilon;
    
    integer delta_index;
    
    reg start_inverse;
    
    wire finished;
    
    wire nextStep;
    
    wire [MATRIX_SIZE * DATA_WIDTH -1:0] step_za;
            
    assign final_step_za = step_za;
    
   BMI #(
    .DATA_WIDTH(DATA_WIDTH),
    .MATRIX_SIZE(MATRIX_SIZE)
   ) bmi_calculator
   (
        .clk(clk),
        .rst(rst),
        .enable(start_inverse),
        .size(size),
        .currentSize(send_size+1'b1),
        .delta(delta),
        .epsilon(epsilon),
        .nextStep(nextStep),
        .relative_index(relative_index),
        .g_min(g_min),
        .finished(finished),
        .step_za(step_za)
   );
   
   always @(posedge rst or posedge nextStep) 
   begin
        if (rst) 
        begin
            send_size <= 0;
        end 
        else if(nextStep && (send_size < size))
        begin
            send_size <= send_size + 1;
        end
    end
       
    always @(posedge clk)
    begin
        if(rst)
        begin
            sub_index <= 0;
            start_inverse <= 0;
            for (i1 = 0; i1 < MATRIX_SIZE; i1 = i1 + 1) begin
                for (j1 = 0; j1 < MATRIX_SIZE; j1 = j1 + 1) begin
                    Ginv_A_A[i1][j1] <= 0;
                end
            end
            i <= 0;
            j <= 0;
        end
        else if(enable)
        begin
            if(sub_index == size_square)
            begin
                start_inverse <= 1;
            end
            else if((A_set[row] && A_set[col]))
            begin
                Ginv_A_A[i][j] <= ginv_mem;
                if(j == size - 1)
                begin
                    j <= 0;
                    i <= i + 1;
                end
                else 
                begin
                    j <= j + 1;
                end
                sub_index <= sub_index + 1;
            end
        end
    end
        
    always @(*)
    begin
        delta = {MATRIX_SIZE*DATA_WIDTH{1'b0}};
        for(delta_index=0;delta_index<MATRIX_SIZE;delta_index=delta_index+1)
        begin
            delta = {Ginv_A_A[delta_index][send_size],delta[DATA_WIDTH*MATRIX_SIZE-1:DATA_WIDTH]};
        end
    end
    
    assign epsilon = Ginv_A_A[send_size][send_size];
    assign ready = finished;
    

endmodule

module extract_stepG #(
    parameter DATA_WIDTH = 16,
    parameter MATRIX_SIZE = 2 * 4
)(
    input [MATRIX_SIZE-1:0] A_set,
    input [DATA_WIDTH-1:0] ginv_mem,
    input [DATA_WIDTH-1:0] size,
    input enable,
    input rst,
    input [DATA_WIDTH-1:0] row,
    input [DATA_WIDTH-1:0] col,
    input clk,
    input [MATRIX_SIZE * DATA_WIDTH -1:0] step_za,
    output reg [DATA_WIDTH*MATRIX_SIZE-1:0] step_g,
    output reg done,
    input [DATA_WIDTH*MATRIX_SIZE-1:0] entire_column
);

    localparam END_INDEX = (DATA_WIDTH-1) + ((DATA_WIDTH-1)/2);
    localparam START_INDEX = (DATA_WIDTH-1) - ((DATA_WIDTH-1)/2) ;
    localparam HALF = DATA_WIDTH / 2;
        
    reg extracted;
    
    reg [DATA_WIDTH-1:0] Ginv__A [0:MATRIX_SIZE-1][0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] step_g_vector [0:MATRIX_SIZE-1];
    
    
    reg sign,step_calculate;
    
    reg finall;
    reg [DATA_WIDTH-1:0] firstTerm;
    
    reg [DATA_WIDTH-1:0] step_za_vector [0:MATRIX_SIZE-1];

    reg [DATA_WIDTH-1:0] Sub_index;
    integer int [0:MATRIX_SIZE-1];
    
    genvar f;
    generate
    for (f=0;f<MATRIX_SIZE;f=f+1) begin : load_Ginv__A
        always @(posedge clk)
        begin
            if(rst)
            begin
                step_g[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] <= 0;
                for(int[f]=0;int[f]<MATRIX_SIZE;int[f]=int[f]+1)
                begin
                    Ginv__A[f][int[f]] <= 0;
                end
                if(f == MATRIX_SIZE-1) 
                begin
                    Sub_index <= 0;
                    extracted <= 0;
                    done <= 0;
                end
            end
            else if(enable)
            begin
                if(!finall && !extracted)
                begin
                    if(A_set[col])
                    begin
                        Ginv__A[f][Sub_index] <= entire_column[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH];
                        if(f == MATRIX_SIZE-1) 
                        begin
                            Sub_index <= Sub_index + 1;
                            if(Sub_index == size - 1) extracted <= 1;
                        end
                    end
                end
                else if(finall && !done)
                begin
                    step_g[(f+1)*DATA_WIDTH-1:f*DATA_WIDTH] <= step_g_vector[f];
                    if(f == MATRIX_SIZE-1) done <= 1;
                end
            end
        end
    end
    endgenerate
    
    
    reg once;
    genvar d;
    
    generate
        for (d = 0; d < MATRIX_SIZE; d = d + 1) begin : step_za_unpack
            always @ (posedge clk) begin
                    if(rst)
                    begin
                        step_za_vector[d] = {DATA_WIDTH{1'b0}};
                        if(d == MATRIX_SIZE - 1)
                        begin
                            step_calculate <= 0;
			    once <= 1;
                        end
                    end
                    else if(extracted & once)
                    begin
                        step_za_vector[d] = step_za[(d+1)*DATA_WIDTH-1 : d*DATA_WIDTH];
                        if(step_za_vector[d] == {1'b1, {DATA_WIDTH-1{1'b0}}})
                        begin
                            step_za_vector[d] = {DATA_WIDTH{1'b0}};
                        end
                        if(d == MATRIX_SIZE - 1)
                        begin
                             step_calculate <= 1;
			     once <= 0;
                        end
                    end
            end  
        end
    endgenerate
    
    wire [DATA_WIDTH-1:0] first_Term [0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] current_Term [0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] new_Term [0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] column [0:MATRIX_SIZE-1];
    reg Sign [0:MATRIX_SIZE-1];
    wire [2*(DATA_WIDTH-1)-1:0] product [0:MATRIX_SIZE-1];
    reg wait_once [0:MATRIX_SIZE-1];
    
    genvar e;
    generate 
        for(e=0;e<MATRIX_SIZE;e=e+1) begin : vector_g_calculate
            assign first_Term[e] = Ginv__A[e][column[e]];
            assign product[e] = first_Term[e][DATA_WIDTH-2:0] * step_za_vector[column[e]][DATA_WIDTH-2:0];
            always @(posedge clk)
            begin
                if(rst)
                begin
                    step_g_vector[e] <= {DATA_WIDTH{1'b0}};
                    column[e] <= {DATA_WIDTH{1'b0}};
                    wait_once[e] <= 1;
                    if(e == MATRIX_SIZE - 1)
                    begin
                        finall <= 0;
                    end
                end
                else if(step_calculate)
                begin
                    if(column[e] <= size)
                    begin
                        new_Term[e] <= {1 ^ Ginv__A[e][column[e]][DATA_WIDTH-1] ^ step_za_vector[column[e]][DATA_WIDTH-1], product[e][END_INDEX : START_INDEX]};
                        column[e] <= column[e] + 1;
                        if(wait_once[e])
                        begin
                            wait_once[e] <= 0;  
                        end
                        else 
                        begin
                            Sign[e] = step_g_vector[e][DATA_WIDTH-1] ^ new_Term[e][DATA_WIDTH-1];
                            if(Sign[e] == 0)
                            begin
                                step_g_vector[e] <= {step_g_vector[e][DATA_WIDTH-1],step_g_vector[e][DATA_WIDTH-2:0] + new_Term[e][DATA_WIDTH-2:0]};
                            end
                            else
                            begin
                                if(step_g_vector[e][DATA_WIDTH-2:0] >= new_Term[e][DATA_WIDTH-2:0])
                                begin
                                    step_g_vector[e] <= {step_g_vector[e][DATA_WIDTH-1],step_g_vector[e][DATA_WIDTH-2:0] - new_Term[e][DATA_WIDTH-2:0]};
                                end
                                else
                                begin
                                    step_g_vector[e] <= {new_Term[e][DATA_WIDTH-1],new_Term[e][DATA_WIDTH-2:0] - step_g_vector[e][DATA_WIDTH-2:0]};
                                end
                            end
                        end
                    end
                    else if(e == MATRIX_SIZE - 1)
                    begin
                        finall <= 1;
                    end
                end
            end
        end
    endgenerate
    
    
endmodule


module extract_step_z_gbcd #(
    parameter DATA_WIDTH = 16
    )(
        input clk,
        input rst,
        input enable,
        input [DATA_WIDTH-1:0] ginv_mem,
        input [DATA_WIDTH-1:0] g_mem,
        input [2:0] index,
        output reg finished,
        output reg [DATA_WIDTH-1:0] z1,
        output reg [DATA_WIDTH-1:0] z2
    );
    
    
    localparam END_INDEX = (DATA_WIDTH-1) + ((DATA_WIDTH-1)/2);
    localparam START_INDEX = (DATA_WIDTH-1) - ((DATA_WIDTH-1)/2) ;
    localparam HALF = DATA_WIDTH / 2;
    
    reg [DATA_WIDTH-1:0] a,b,c,d,g1,g2, dividend,divisor,reciprocal;
    
    wire [2*(DATA_WIDTH-1)-1:0] rp0_0,rp0_1,rp1_0,rp1_1,rp2_0,rp2_1,rp3_0,rp3_1;
    reg calculate_inverse,sign;
    reg [1:0] state,operation;
    wire [DATA_WIDTH-1:0] result,division_result;
    wire [DATA_WIDTH-1:0] firstTerm0,secondTerm0;
    reg [DATA_WIDTH-1:0] firstTerm1,secondTerm1,firstTerm2,secondTerm2;
    
    assign rp0_0 = a[DATA_WIDTH-2:0] * d[DATA_WIDTH-2:0];
    assign rp0_1 = b[DATA_WIDTH-2:0] * c[DATA_WIDTH-2:0];
    assign firstTerm0 = {a[DATA_WIDTH-1]^d[DATA_WIDTH-1],rp0_0[END_INDEX:START_INDEX]};
    assign secondTerm0 = {1^b[DATA_WIDTH-1]^c[DATA_WIDTH-1],rp0_1[END_INDEX:START_INDEX]};
    
    assign rp1_0 = d[DATA_WIDTH-2:0] * g1[DATA_WIDTH-2:0];
    assign rp1_1 = b[DATA_WIDTH-2:0] * g2[DATA_WIDTH-2:0];
    
    assign rp2_0 = c[DATA_WIDTH-2:0] * g1[DATA_WIDTH-2:0];
    assign rp2_1 = a[DATA_WIDTH-2:0] * g2[DATA_WIDTH-2:0];
    
    assign rp3_0 = z1[DATA_WIDTH-2:0] * reciprocal[DATA_WIDTH-2:0];
    assign rp3_1 = z2[DATA_WIDTH-2:0] * reciprocal[DATA_WIDTH-2:0];
    
   wire div_enable, divided;
   assign div_enable = operation == 3;
   
   fixed_division #(.WIDTH(DATA_WIDTH)) 
        divisioner (
            .clk(clk),
            .enable(div_enable),
            .rst(rst),
            .a(dividend),
            .b(divisor),
            .result(division_result),
            .finished(divided)
        );
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            a <= 0;
            b <= 0;
            c <= 0;
            d <= 0;
            g1 <= 0;
            g2 <= 0;
            calculate_inverse <= 0;
            state <= 0;
            dividend <= (1 << HALF);
            divisor = (1 << HALF);
            operation = 0;
            reciprocal <= 0;
            z1 = 0;
            z2 = 0;
            finished <= 0;
        end
        if(enable)
        begin
            if(!calculate_inverse)
            begin
                case (index)
                    3'b000: a <= ginv_mem;
                    3'b001: b <= ginv_mem;
                    3'b010: c <= ginv_mem;
                    3'b011: d <= ginv_mem;
                    3'b100: g1 <= g_mem;
                    3'b101: 
                        begin
                            g2 <= g_mem;
                            calculate_inverse <= 1;
                       end
                endcase
            end
            else if(calculate_inverse && !finished)
            begin
                if(state == 0)
                begin
                    sign = firstTerm0[DATA_WIDTH-1] ^ secondTerm0[DATA_WIDTH-1];
                    if(sign==0)
                    begin
                        divisor = {firstTerm0[DATA_WIDTH-1],firstTerm0[DATA_WIDTH-2:0]+secondTerm0[DATA_WIDTH-2:0]};
                    end
                    else
                    begin
                        if(firstTerm0[DATA_WIDTH-2:0] >= secondTerm0[DATA_WIDTH-2:0])
                        begin
                            divisor = {firstTerm0[DATA_WIDTH-1],firstTerm0[DATA_WIDTH-2:0] - secondTerm0[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                           divisor = {secondTerm0[DATA_WIDTH-1],secondTerm0[DATA_WIDTH-2:0] - firstTerm0[DATA_WIDTH-2:0]};
                        end     
                    end
                    state <= 1;
                    firstTerm1 <= {d[DATA_WIDTH-1]^g1[DATA_WIDTH-1],rp1_0[END_INDEX:START_INDEX]};
                    secondTerm1 <= {1^b[DATA_WIDTH-1]^g2[DATA_WIDTH-1],rp1_1[END_INDEX:START_INDEX]};
                    operation = 3;
                end
                else if(state == 1)
                begin
                    sign = firstTerm1[DATA_WIDTH-1] ^ secondTerm1[DATA_WIDTH-1];
                    if(sign==0)
                    begin
                        z1 = {firstTerm1[DATA_WIDTH-1],firstTerm1[DATA_WIDTH-2:0]+secondTerm1[DATA_WIDTH-2:0]};
                    end
                    else
                    begin
                        if(firstTerm1[DATA_WIDTH-2:0] >= secondTerm1[DATA_WIDTH-2:0])
                        begin
                            z1 = {firstTerm1[DATA_WIDTH-1],firstTerm1[DATA_WIDTH-2:0] - secondTerm1[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                           z1 = {secondTerm1[DATA_WIDTH-1],secondTerm1[DATA_WIDTH-2:0] - firstTerm1[DATA_WIDTH-2:0]};
                        end     
                    end
                    state <= 2;
                    firstTerm2 <= {1^c[DATA_WIDTH-1]^g1[DATA_WIDTH-1],rp2_0[END_INDEX:START_INDEX]};
                    secondTerm2 <= {a[DATA_WIDTH-1]^g2[DATA_WIDTH-1],rp2_1[END_INDEX:START_INDEX]};
                end
                else if(state == 2)
                begin
                    if(divided)
                    begin
                        reciprocal <= division_result;
                        sign = firstTerm2[DATA_WIDTH-1] ^ secondTerm2[DATA_WIDTH-1];
                        if(sign==0)
                        begin
                            z2 = {firstTerm2[DATA_WIDTH-1],firstTerm2[DATA_WIDTH-2:0]+secondTerm2[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                            if(firstTerm2[DATA_WIDTH-2:0] >= secondTerm2[DATA_WIDTH-2:0])
                            begin
                                z2 = {firstTerm2[DATA_WIDTH-1],firstTerm2[DATA_WIDTH-2:0] - secondTerm2[DATA_WIDTH-2:0]};
                            end
                            else
                            begin
                               z2 = {secondTerm2[DATA_WIDTH-1],secondTerm2[DATA_WIDTH-2:0] - firstTerm2[DATA_WIDTH-2:0]};
                            end     
                        end
                        operation = 0;
                        state <= 3;
                    end
                end
                else if(state == 3)
                begin
                    z1 = {z1[DATA_WIDTH-1]^reciprocal[DATA_WIDTH-1],rp3_0[END_INDEX:START_INDEX]};
                    z2 = {z2[DATA_WIDTH-1]^reciprocal[DATA_WIDTH-1],rp3_1[END_INDEX:START_INDEX]};
                    finished <= 1;
                end
            end
        end
    end
    
    
endmodule

module CICS #(
    parameter DATA_WIDTH = 16,
    parameter USERS = 2 * 4,
    parameter ANTENNAS = 2 * 4
)(
    input clk,
    input rst,
    input start_calculation,
    input [31:0] modulation,
    input [2:0] nmax,
    output reg [DATA_WIDTH-1:0] row,
    output reg [DATA_WIDTH-1:0] col,
    input [DATA_WIDTH-1:0] S_out,
    input [DATA_WIDTH-1:0] Ginv_out,
    input [DATA_WIDTH-1:0] g_mem_output_0,
    input [DATA_WIDTH-1:0] g_mem_output_1,
    input g_written_0,
    input g_written_1,
    output gradient_enable,
    output reg [DATA_WIDTH-1:0] gradient_index,
    output [DATA_WIDTH*USERS-1:0] step_g,
    output update_step_g,
    output completed,
    output reset_values,
    output reg i_selector,
    input GBCD_not_busy,
    output waiting,
    output crossed,
    output reg can_trigger,
    input [DATA_WIDTH*USERS-1:0] entire_column,
    input [DATA_WIDTH*USERS-1:0] entire_symbols
    );
    
    wire g_written;
    assign g_written = i_selector ? g_written_1 : g_written_0;
    wire [DATA_WIDTH-1:0] g_mem_output;
    assign g_mem_output = i_selector ? g_mem_output_1 : g_mem_output_0;
    
    localparam HALF = DATA_WIDTH / 2;
    
    integer outer;
    
    wire find_min_value,load_submatrix, calculate_step_g;
    
    wire [DATA_WIDTH-1:0] current_minIndex;
    wire [DATA_WIDTH-1:0] minValue;
    wire [USERS-1:0] updated_A;
    reg [DATA_WIDTH-1:0] relative_index;
    
    wire [DATA_WIDTH-1:0] g_value;
    integer p,c,count;
    reg [DATA_WIDTH-1:0] submatrix_size;
    
    wire inverse_extracted,step_g_extracted;
    wire [DATA_WIDTH*USERS-1:0] step_za;
    reg calculate_step_z;
    
    reg check_CI_constraints;
    
    reg sign2; reg [DATA_WIDTH-1:0] temp [0:USERS-1];
    wire sign;
    
    reg [DATA_WIDTH-1:0] ordered_step_za [0:USERS-1],un_ordered_step_za [0:USERS-1],z [0:USERS-1];

    reg [USERS-1:0] A,B;
    
    reg [2:0] iteration;
    reg reset;
    
    reg [DATA_WIDTH-1:0] size_square;
    
    min_value_finder #(
        .DATA_WIDTH(DATA_WIDTH),
        .USERS(USERS)
    ) CICS_UPDATE_A
    (
        .A(A),
        .B(B),
        .find_min_value(find_min_value),
        .clk(clk),
        .rst(rst | reset_values | reset),
        .index(gradient_index),
        .g_value(g_value),
        .updated_A(updated_A),
        .minValue(minValue),
        .min_index(current_minIndex)
    );
    
    extract_submatrix #(
        .DATA_WIDTH(DATA_WIDTH),
        .MATRIX_SIZE(USERS)
    ) CICS_MATRIX_M
    (
        .A_set(A),
        .size(submatrix_size),
        .size_square(size_square),
        .clk(clk),
        .rst(rst | reset_values | reset),
        .ginv_mem(Ginv_out),
        .g_min(g_mem_output),
        .row(row),
        .col(col),
        .enable(load_submatrix),
        .relative_index(relative_index),
        .ready(inverse_extracted),
        .final_step_za(step_za)
    );
    
    extract_stepG #(
        .DATA_WIDTH(DATA_WIDTH),
        .MATRIX_SIZE(USERS)
    ) step_g_calculator
    (
        .A_set(A),
        .size(submatrix_size),
        .clk(clk),
        .rst(rst | reset_values | reset),
        .ginv_mem(Ginv_out),
        .row(row),
        .col(col),
        .enable(calculate_step_g),
        .step_za(step_za),
        .step_g(step_g),
        .done(step_g_extracted),
        .entire_column(entire_column)
    );
    
    reg [3:0] state;
        
    assign gradient_enable = state == 1;
    assign find_min_value = state == 2;
    assign load_submatrix = state == 4 | state == 5;
    assign reset_values = state == 12 | state == 13;
    assign calculate_step_g = state == 8 | state == 9;
    assign update_step_g = state == 10;
    assign completed = state == 14;
    assign waiting = state == 15;
    
    assign crossed = iteration > nmax / 2;
    
    assign sign = z[col][DATA_WIDTH-1] ^ ordered_step_za[col][DATA_WIDTH-1];

    reg gradient_wait;
    
    
    wire Sign [0:USERS-1];
    reg [0:USERS-1] Satisfy;
    wire SATISFY; assign SATISFY = &Satisfy;
    
    wire [DATA_WIDTH-1:0] SOUT [0:USERS-1];
    
    genvar e;
    generate
        for(e=0;e<USERS;e=e+1) begin : temp_calculate
            assign Sign[e] = z[e][DATA_WIDTH-1] ^ ordered_step_za[e][DATA_WIDTH-1];
            assign SOUT[e] = entire_symbols[(e+1)*DATA_WIDTH-1:e*DATA_WIDTH];
            always @(posedge clk)
            begin
                if(rst)  
                begin
                    temp[e] <= 0;
                    Satisfy[e] <= 1;
                    B[e] <= 0;
                end
                else
                begin
                    case (state)
                        0:
                        begin
                            z[e] <= SOUT[e];
                            if(SOUT[e][DATA_WIDTH-2:HALF] < outer)
                            begin
                                B[e] <= 1;
                            end
                        end
                        6:
                        begin
                            if(A[e])
                            begin
                                if(!Sign[e])
                                begin
                                        temp[e] <= {z[e][DATA_WIDTH-1],z[e][DATA_WIDTH-2:0] + ordered_step_za[e][DATA_WIDTH-2:0]};
                                end
                                else
                                begin
                                    if(z[e][DATA_WIDTH-2:0] >= ordered_step_za[e][DATA_WIDTH-2:0])
                                    begin
                                            temp[e] <= {z[e][DATA_WIDTH-1],z[e][DATA_WIDTH-2:0] - ordered_step_za[e][DATA_WIDTH-2:0]};
                                    end
                                    else                            
                                    begin
                                            temp[e] <= {ordered_step_za[e][DATA_WIDTH-1],ordered_step_za[e][DATA_WIDTH-2:0] - z[col][DATA_WIDTH-2:0]};
                                    end
                                end
                            end
                        end
                        7:
                        begin
                            if(A[e]) 
                            begin
                                if((SOUT[e][DATA_WIDTH-2:HALF]) < outer)
                                begin
                                    if(temp[e] != SOUT[e])
                                    begin
                                        Satisfy[e] <= 0;
                                    end
                                end
                                else
                                begin
                                    if(temp[e][DATA_WIDTH-1] && temp[e] > SOUT[e])
                                    begin
                                        Satisfy[e] <= 0;
                                    end
                                    else if(!temp[e][DATA_WIDTH-1] && !SOUT[e][DATA_WIDTH-1] && temp[e] < SOUT[e])
                                    begin
                                        Satisfy[e] <= 0;
                                    end
                                end  
                            end
                        end
                        11:
                        begin
                            if(A[e])
                            begin
                                z[e] <= temp[e];
                            end
                        end
                        12:
                        begin
                            if(!SATISFY && e == current_minIndex)
                            begin
                                B[e] <= 1;
                            end
                        end
                        13:
                        begin
                            Satisfy[e] <= 1;
                            if(iteration >= nmax)
                            begin
                                B[e] <= 0;
                            end
                        end
                        14:
                        begin
                            if(reset) 
                            begin
                                B[e] <= 0;
                            end
                        end
                    endcase
                end
            end
        end
    endgenerate

    reg cycle;
    reg satsified_before;

    always @(posedge clk)
    begin
        if(rst)
        begin
            state <= 15;
            col <= {DATA_WIDTH{1'b0}};
            row <= {DATA_WIDTH{1'b0}};
            A <= {USERS{1'b0}};
            gradient_index <= {DATA_WIDTH{1'b0}};
            submatrix_size <= {DATA_WIDTH{1'b0}};
            iteration <= 0;
            i_selector <= 0;
            can_trigger <= 0;
            reset <= 0;
            gradient_wait <= 0;
            size_square <= {DATA_WIDTH{1'b0}};
            cycle <= 0;
            satsified_before <= 0;
        end
        else
        begin
            case (state)
            0:
            begin
                state <= 1;
            end
            1:
            begin
                if(col <= USERS - 1)
                begin
                    col <= col + 1;
                end
                else
                begin
                    col <= {DATA_WIDTH{1'b0}};
                    state <= 2;
                end
            end
            2:
            begin
                if(gradient_index < USERS) // prev USERS - 1
                begin
                    gradient_index <= gradient_index + 1;
                    col <= col + 1;
                end
                else
                begin
                    A <= updated_A;
                    submatrix_size <= {DATA_WIDTH{1'b0}};
                    p <= 0;
                    relative_index <= {DATA_WIDTH{1'b0}};
                    row <= {DATA_WIDTH{1'b0}};
                    col <= {DATA_WIDTH{1'b0}};
                    if((minValue[DATA_WIDTH-1] == 0)  | (minValue[DATA_WIDTH-2:0] == 0))
                    begin
                        gradient_index <= 0;
                        state <= 14;
                        reset <= 1;
                        iteration <= nmax;
                    end
                    else 
                    begin
                        gradient_index <= current_minIndex;
                        state <= 3;
                    end
                end
            end
            3:
            begin
                if(!satsified_before)
                begin
                    size_square <= {{(DATA_WIDTH-1){1'b0}}, 1'b1};
                    submatrix_size <= {{(DATA_WIDTH-1){1'b0}}, 1'b1};
                    col <= current_minIndex;
                    row <= current_minIndex;
                    state <= 5;
                end
                else
                begin
                    if(p<USERS) 
                    begin
                        if(A[p]) 
                        begin
                            submatrix_size <= submatrix_size + 1;
                            if(p < current_minIndex)
                            begin
                                relative_index <= relative_index + 1;
                            end
                        end
                        p <= p + 1;
                    end
                    else 
                    begin
                        size_square <= submatrix_size * submatrix_size;
                        if(submatrix_size == 1)
                        begin
                            col <= current_minIndex;
                            row <= current_minIndex;
                            state <= 5;
                        end
                        else
                        begin
                            state <= 4;        
                        end
                    end
                end
            end
            4:
            begin
                if(col < USERS - 1)
                begin
                    col <= col + 1;
                end
                else
                begin
                    if(row == USERS-1)
                    begin
                        state <= 5;
                    end
                    else
                    begin
                        row <= row + 1;
                        col <= {DATA_WIDTH{1'b0}};
                    end
                end
            end
            5:
            begin
                if(check_CI_constraints)
                begin
                    row <= {DATA_WIDTH{1'b0}};
                    col <= {DATA_WIDTH{1'b0}};
                    state <= 6;
                end
            end
            6:
            begin
                gradient_index <= {DATA_WIDTH{1'b0}};
                state <= 7;
		        cycle <= 0; 
            end
            7:
            begin
                cycle <= cycle + 1;
                if(cycle)
                begin
                    if(SATISFY)
                    begin
                        satsified_before <= 1;
                        if(submatrix_size == 1)
                        begin
                            col <= current_minIndex;
                            state <= 9;
                        end
                        else
                        begin
                            state <= 8;
                        end
                    end
                    else
                    begin
                        state <= 12;
                    end
                end
            end
            8:
            begin
                if(col < USERS - 1)
                begin
                    col <= col + 1;
                end
                else
                begin
                    col <= {DATA_WIDTH{1'b0}};
                    state <= 9;
                end
            end
            9:
            begin
                if(step_g_extracted)
                begin
                    state <= 10;
                    col <= {DATA_WIDTH{1'b0}};
                end
            end
            10:
            begin
                state <= 11;
            end
            11:
            begin
                state <= 12;
            end
            12:
            begin
                if(!SATISFY)
                begin
                    A <= A & (~(1<<current_minIndex));
                end
                state <= 13;
            end
            13:
            begin
                if(iteration < nmax)
                begin
                    iteration <= iteration + 1;
                    state <= 2;
                end
                else
                begin
                    state <= 14;
                    A <= {USERS{1'b0}};
                end
            end
            14:
            begin
                if(reset)
                begin
                    reset <= 0;
                    A <= {USERS{1'b0}};
                end
                if(GBCD_not_busy)
                begin
                    i_selector <= i_selector + 1;
                    state <= 15;
                end
            end
            15:
            begin
                if(start_calculation)
                begin
                    state <= 0;
                    can_trigger <= can_trigger + 1;
                    iteration <= 0;
                    satsified_before <= 0;
                end
            end
        endcase
        end
    end
    
    always @(*)
    begin
        outer = modulation == 16 ? 3 : modulation == 64 ? 7 : 1;
    end
    
    assign g_value = {S_out[DATA_WIDTH-1] ^ g_mem_output[DATA_WIDTH-1], g_mem_output[DATA_WIDTH-2:0]}; // based on which we calculate the minimum index for set A
    
    
    genvar d;
    
    generate
        for (d = 0; d < USERS; d = d + 1) begin : step_za_unpack
            always @ (inverse_extracted, rst) 
            begin
                if(rst)
                begin
                    un_ordered_step_za[d] = {DATA_WIDTH{1'b0}};
                end
                else if(inverse_extracted)
                begin
                    un_ordered_step_za[d] = step_za[(d+1)*DATA_WIDTH-1 : d*DATA_WIDTH];
                    if(un_ordered_step_za[d] == {1'b1, {DATA_WIDTH-1{1'b0}}})
                    begin
                        un_ordered_step_za[d] = {DATA_WIDTH{1'b0}};
                    end
                end
            end
        end
    endgenerate

     
    always @ (inverse_extracted) 
    begin
        if(inverse_extracted)
        begin
                calculate_step_z <= 1;
        end
        else calculate_step_z <= 0;
    end
    
    always @(rst or calculate_step_z or reset_values)
    begin
        if(rst | reset_values)
        begin
            check_CI_constraints <= 0;
        end
        else if(calculate_step_z)
        begin
            count = 0;
            for(c=0;c<USERS;c=c+1)
            begin
                if(A[c] == 1)
                begin
                    ordered_step_za[c] = un_ordered_step_za[count];
                    count = count + 1;
                end
                else
                begin
                    ordered_step_za[c] = 0;
                end
            end
            check_CI_constraints <= 1;   
        end
    end    
    
endmodule


module BMI #(
    parameter DATA_WIDTH = 16,
    parameter MATRIX_SIZE = 2 * 4
)(
    input enable,
    input clk,
    input rst,
    input [DATA_WIDTH-1:0] size,
    input [DATA_WIDTH-1:0] currentSize,
    input [DATA_WIDTH-1:0] epsilon,
    input [MATRIX_SIZE * DATA_WIDTH -1:0] delta,
    input [DATA_WIDTH-1:0] relative_index,
    input [DATA_WIDTH-1:0] g_min,
    output reg nextStep,
    output reg finished,
    output reg [DATA_WIDTH*MATRIX_SIZE-1:0] step_za
    );
    
    
    localparam END_INDEX = (DATA_WIDTH-1) + ((DATA_WIDTH-1)/2);
    localparam START_INDEX = (DATA_WIDTH-1) - ((DATA_WIDTH-1)/2) ;
    localparam HALF = DATA_WIDTH / 2;
    
    reg [DATA_WIDTH-1:0] delta_vector [0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] t_vector [0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] t_t [0:MATRIX_SIZE-1][0:MATRIX_SIZE-1];
    reg [DATA_WIDTH-1:0] reciprocal_u;
    
    reg sign,sign1;
    reg [2*(DATA_WIDTH-1)-1:0] rp5 [0:MATRIX_SIZE-1];
    
    reg [DATA_WIDTH-1:0] inverse_matrix [0:MATRIX_SIZE-1][0:MATRIX_SIZE-1];
    
    integer k,j,k1,j1;
    
    reg [3:0] state;
    
    reg [DATA_WIDTH-1:0] a,b;
    
    wire [DATA_WIDTH-1:0] result,division_result;
    
    reg [1:0] operation;
    
    reg [DATA_WIDTH-1:0] newT, currentT; // for t = Sinv * delta update
    
    reg [DATA_WIDTH-1:0] new_d_Sinv_d, current_d_Sinv_d; // for delta.T * Sinv * delta update 
    
    wire [DATA_WIDTH-1:0] new_inv, current_inv; // for Sinv + t.T update
    
    
    reg extract_step_za;
    
    reg c;
    
    wire [DATA_WIDTH-1:0] firstTerm;
    wire [2*(DATA_WIDTH-1)-1:0] rp0,rp1,rp2,rp3,rp4;
    
    wire  [31:0] state1,state2,state3,state5;
    assign state1 = state == 1 ? 32'b1 : 32'b0;
    assign state2 = state == 2 ? 32'b1 : 32'b0;
    assign state3 = state == 3 ? 32'b1 : 32'b0;
    assign state5 = state == 5 ? 32'b1 : 32'b0;
    
    assign firstTerm = inverse_matrix[j & state1][k & state1];
    assign rp0 = firstTerm[DATA_WIDTH-2:0] * delta_vector[k & state1][DATA_WIDTH-2:0];
    assign rp1 = delta_vector[j & state2][DATA_WIDTH-2:0] * t_vector[j & state2][DATA_WIDTH-2:0];
    assign rp2 = t_vector[j & state3][DATA_WIDTH-2:0] * t_vector[k & state3][DATA_WIDTH-2:0];
    assign rp3 = t_t[j & state5][k & state5][DATA_WIDTH-2:0] * reciprocal_u[DATA_WIDTH-2:0];
    assign rp4 = t_vector[j & state5][DATA_WIDTH-2:0] * reciprocal_u[DATA_WIDTH-2:0];
    assign current_inv = inverse_matrix[j][k];
    assign new_inv = t_t[j][k];

    
    wire div_enable;
    reg div_rst;
    wire divided;
    assign div_enable = operation == 3;
    
    reg make_zero;
    
    fixed_division #(.WIDTH(DATA_WIDTH)) 
        divisioner (
            .clk(clk),
            .enable(div_enable),
            .rst(rst | div_rst),
            .a(a),
            .b(b),
            .result(division_result),
            .finished(divided)
        );
    
    genvar i;
    
    generate
        for (i = 0; i < MATRIX_SIZE; i = i + 1) begin : delta_unpack
            always @ (posedge clk) begin
                if (enable) begin
                    delta_vector[i] <= delta[(i+1)*DATA_WIDTH-1 : i*DATA_WIDTH];
                end
                else begin
                    delta_vector[i] <= 0;
                end
            end
        end
    endgenerate
    
    always @(posedge clk)
    begin
        if(rst)
        begin
            for (j1 = 0; j1 < MATRIX_SIZE; j1 = j1 + 1) begin
                for (k1 = 0; k1 < MATRIX_SIZE; k1 = k1 + 1) begin
                    t_t[j1][k1] <= {DATA_WIDTH{1'b0}};
		    inverse_matrix[j1][k1] = {DATA_WIDTH{1'b0}};
                end
            end
            state <= 4'd0;
            a <= (1 << HALF);
            b <= (1 << HALF);
            operation <= 0;
            nextStep <= 0;
            reciprocal_u <= (1 << HALF);
            extract_step_za <= 0;
            c <= 0;
            k <= 0;
            j <= 0;
            div_rst <= 0;
            make_zero <= 1;
        end
        else if(enable && !finished)
        begin
            case (state)
            0:
            begin
                if(currentSize == 1)
                begin
                    a <= (1 << HALF);
                    b <= epsilon;
                    operation <= 3;
                end
                if(operation == 3 && divided)
                begin
                    state <= 7; 
                end
            end
            1:
            begin
                if(j==0 && k==0)
                begin
                    nextStep <= 0;
                    operation <= 0;
                    div_rst <= 0;
                end
                if(k<currentSize)
                begin
                    newT <= {inverse_matrix[j][k][DATA_WIDTH-1] ^ delta_vector[k][DATA_WIDTH-1], rp0[END_INDEX : START_INDEX]};
                    k <= k + 1;
                    if(k==0 & make_zero)
                    begin
                        t_vector[j] <= {DATA_WIDTH{1'b0}};
                        make_zero <= 0;
                    end
                    else
                    begin
                    sign = t_vector[j][DATA_WIDTH-1] ^ newT[DATA_WIDTH-1];
                    if(sign == 0)
                    begin
                        t_vector[j] <= {t_vector[j][DATA_WIDTH-1],t_vector[j][DATA_WIDTH-2:0] + newT[DATA_WIDTH-2:0]};
                    end
                    else
                    begin
                       if(t_vector[j][DATA_WIDTH-2:0] >= newT[DATA_WIDTH-2:0])
                       begin
                            t_vector[j] <= {t_vector[j][DATA_WIDTH-1],t_vector[j][DATA_WIDTH-2:0] - newT[DATA_WIDTH-2:0]};
                       end
                       else
                       begin
                           t_vector[j] <= {newT[DATA_WIDTH-1],newT[DATA_WIDTH-2:0] - t_vector[j][DATA_WIDTH-2:0]};
                       end 
                    end
                    end
                end
                else
                begin
                    k <= 0;
                    make_zero <= 1;
                    if(j == currentSize-1)
                    begin
                        state <= 2;
                        j <= 0;
                    end
                    else begin j <= j + 1; end
                end
            end
            2:
            begin
                if(j<currentSize)
                begin
                    new_d_Sinv_d <= {delta_vector[j][DATA_WIDTH-1] ^ t_vector[j][DATA_WIDTH-1], rp1[END_INDEX : START_INDEX]};
                    j <= j + 1; 
                    if(j==0 & make_zero)
                    begin
                        current_d_Sinv_d <= epsilon;
                        make_zero <= 0;
                    end
                    else begin
                    if(current_d_Sinv_d[DATA_WIDTH-1] == 0 && new_d_Sinv_d[DATA_WIDTH-1] == 0)
                    begin
                        if(current_d_Sinv_d[DATA_WIDTH-2:0] >= new_d_Sinv_d[DATA_WIDTH-2:0])
                        begin
                            current_d_Sinv_d <= {1'b0,current_d_Sinv_d[DATA_WIDTH-2:0] - new_d_Sinv_d[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                            current_d_Sinv_d <= {1'b1,new_d_Sinv_d[DATA_WIDTH-2:0] - current_d_Sinv_d[DATA_WIDTH-2:0]};
                        end
                    end
                    else if(current_d_Sinv_d[DATA_WIDTH-1] == 0 && new_d_Sinv_d[DATA_WIDTH-1] == 1)
                    begin
                        current_d_Sinv_d <= {1'b0,current_d_Sinv_d[DATA_WIDTH-2:0] + new_d_Sinv_d[DATA_WIDTH-2:0]};
                    end
                    else if(current_d_Sinv_d[DATA_WIDTH-1] == 1 && new_d_Sinv_d[DATA_WIDTH-1] == 0)
                    begin
                        current_d_Sinv_d <= {1'b1,current_d_Sinv_d[DATA_WIDTH-2:0] + new_d_Sinv_d[DATA_WIDTH-2:0]};
                    end
                    else if(current_d_Sinv_d[DATA_WIDTH-1] == 1 && new_d_Sinv_d[DATA_WIDTH-1] == 1)
                    begin
                        if(current_d_Sinv_d[DATA_WIDTH-2:0] >= new_d_Sinv_d[DATA_WIDTH-2:0])
                        begin
                            current_d_Sinv_d <= {1'b1,current_d_Sinv_d[DATA_WIDTH-2:0] - new_d_Sinv_d[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                            current_d_Sinv_d <= {1'b0,new_d_Sinv_d[DATA_WIDTH-2:0] - current_d_Sinv_d[DATA_WIDTH-2:0]};
                        end
                    end
                    end
                end
                else
                begin
                    a <= (1 << HALF);
                    b <= current_d_Sinv_d;
                    operation <= 3;
                    state <= 3;
                    j <= 0;
                    make_zero <= 1;
                end
            end
            3:
            begin
                if(k<currentSize)
                begin
                    t_t[j][k] <= {t_vector[j][DATA_WIDTH-1] ^ t_vector[k][DATA_WIDTH-1],rp2[END_INDEX:START_INDEX]};
                    k <= k + 1;
                end
                else
                begin
                    k <= 0;
                    if(j == currentSize - 1)
                    begin
                        j <= 0;
                        state <= 4;
                    end
                    else begin j <= j + 1; end
                end
            end
            4:
            begin
                if(divided)
                begin
                    reciprocal_u <= division_result;
                    state <= 5;
                    div_rst <= 1;
                end
            end
            5:
            begin
                if(k<currentSize)
                begin
                    if(j==0 && k==0)
                    begin
                        operation <= 0; 
                        div_rst <= 0;
                    end
                    t_t[j][k] <= {t_t[j][k][DATA_WIDTH-1] ^ reciprocal_u[DATA_WIDTH-1],rp3[END_INDEX:START_INDEX]};
                    k <= k + 1;
                end
                else
                begin
                    k <= 0;
                    if(j<currentSize)
                    begin
                        t_vector[j] <= {1 ^ t_vector[j][DATA_WIDTH-1] ^ reciprocal_u[DATA_WIDTH-1],rp4[END_INDEX:START_INDEX]};
                    end
                    if(j==currentSize-1)
                    begin
                         j <= 0;
                        state <= 6;
                    end
                    else begin j <= j + 1; end
                end
            end
            6:
            begin
                if(k<currentSize)
                begin
                    if(k == currentSize - 1 && j == currentSize - 1)
                    begin
                        inverse_matrix[j][k] = reciprocal_u;
                    end
                    else if(k == currentSize - 1)
                    begin
                        inverse_matrix[j][k] = t_vector[j];
                    end
                    else if(j == currentSize - 1)
                    begin
                       inverse_matrix[j][k] = t_vector[k];
                    end
                    else 
                    begin
                        sign1 = current_inv[DATA_WIDTH-1] ^ new_inv[DATA_WIDTH-1];
                        if(sign1 == 0)
                        begin
                            inverse_matrix[j][k] = {current_inv[DATA_WIDTH-1],current_inv[DATA_WIDTH-2:0] + new_inv[DATA_WIDTH-2:0]};
                        end
                        else
                        begin
                            if(current_inv[DATA_WIDTH-2:0] >= new_inv[DATA_WIDTH-2:0])
                            begin
                                inverse_matrix[j][k] = {current_inv[DATA_WIDTH-1],current_inv[DATA_WIDTH-2:0] - new_inv[DATA_WIDTH-2:0]};
                            end
                            else
                            begin
                                inverse_matrix[j][k] = {new_inv[DATA_WIDTH-1],new_inv[DATA_WIDTH-2:0] - current_inv[DATA_WIDTH-2:0]};
                            end
                        end
                    end
                    k <= k + 1;
                end
                else
                begin
                    k <= 0;
                    if(j==currentSize-1)
                    begin  
                        j <= 0; 
                        state <= 7;
                    end
                    else begin j <= j + 1; end
                end
            end
            7:
            begin
                if(div_rst)
                begin
                    div_rst <= 0;
                    operation <= 0;
                end
                if((size == 1 || currentSize == 1) && !c)
                begin
                    inverse_matrix[0][0] = division_result;
                    div_rst <= 1;
                    if(currentSize < size)
                    begin
                        nextStep <= 1;
                    end
                    else if(size == 1)
                    begin
                        operation <= 0;
                        c <= 1;
                    end
                end
                else if(nextStep == 1)
                begin
                    j <= 0;
                    k <= 0;
                    state <= 1;
                    div_rst <= 1;
                    make_zero <= 1;
                end
                else if(currentSize < size)
                begin
                    nextStep <= 1;
                end
                else if( currentSize == size && !extract_step_za)
                begin
                        extract_step_za <= 1;
                end
            end
            endcase
        end
    end

genvar index;
    
    reg wait_cycle [0:MATRIX_SIZE-1];
    
    generate
        for (index = 0; index < MATRIX_SIZE; index = index + 1) begin : step_za_extract
            always @ (posedge clk) begin
                if (rst) begin
                   step_za[(index+1)*DATA_WIDTH-1:index*DATA_WIDTH] <= {DATA_WIDTH{1'b0}};
                   wait_cycle[index] <= 1;
                   rp5[index] <= 0;
                   if(index == MATRIX_SIZE-1)
                   begin
                       finished <= 0;
                   end
                end
                else if(extract_step_za & !finished) begin
                    if(wait_cycle[index]) begin
                        rp5[index] <= inverse_matrix[index][relative_index][DATA_WIDTH-2:0] * g_min[DATA_WIDTH-2:0];
                        wait_cycle[index] <= 0;
                    end
                    else begin
                        step_za[(index+1)*DATA_WIDTH-1:index*DATA_WIDTH] <= {inverse_matrix[index][relative_index][DATA_WIDTH-1] ^ g_min[DATA_WIDTH-1], rp5[index][END_INDEX : START_INDEX]};
                        if(index == MATRIX_SIZE-1)
                        begin
                            finished <= 1;
                        end
                    end
                end
            end
        end
    endgenerate
    
endmodule
