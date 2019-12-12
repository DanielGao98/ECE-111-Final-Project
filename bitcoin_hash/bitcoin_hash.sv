module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] message_addr, output_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] mem_addr,
                    output logic [31:0] mem_write_data,
                     input logic [31:0] mem_read_data);

parameter num_nonces = 16;

enum logic [4:0] {IDLE, READ, PROCESS, WRITE} state;
logic [31:0] hout[num_nonces];
logic [15:0] offset; // in word address
logic [31:0] final_out[8];

logic [31:0] nonce;

logic proc1_rstn, proc2_rstn, proc3_rstn;
logic [31:0] w[19];
logic [31:0] h0_2, h1_2, h2_2, h3_2, h4_2, h5_2, h6_2, h7_2, h0_1, h1_1, h2_1, h3_1, h4_1, h5_1, h6_1, h7_1, 
h0_3, h1_3, h2_3, h3_3, h4_3, h5_3, h6_3, h7_3;
logic [31:0] phase1_out0, phase1_out1, phase1_out2, phase1_out3, phase1_out4, phase1_out5, phase1_out6, phase1_out7,
phase2_out1, phase2_out2, phase2_out3, phase2_out4, phase2_out5, phase2_out6, phase2_out7, phase2_out0, 
phase3_out1, phase3_out2, phase3_out3, phase3_out4, phase3_out5, phase3_out6, phase3_out7;
logic [ 7:0] i, j;

logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;

logic [31:0] zero_word;
logic [31:0] one_word;


logic proc1_e, proc2_e, proc3_e;
logic proc1_done, proc2_done, proc3_done;
// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;

assign h0_2 = phase1_out0;
assign h1_2 = phase1_out1;
assign h2_2 = phase1_out2;
assign h3_2 = phase1_out3;
assign h4_2 = phase1_out4;
assign h5_2 = phase1_out5;
assign h6_2 = phase1_out6;
assign h7_2 = phase1_out7;

assign h0_3 = phase2_out0;
assign h1_3 = phase2_out1;
assign h2_3 = phase2_out2;
assign h3_3 = phase2_out3;
assign h4_3 = phase2_out4;
assign h5_3 = phase2_out5;
assign h6_3 = phase2_out6;
assign h7_3 = phase2_out7;



parameter int k[64] = '{
    32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
    32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
    32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
    32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
    32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
    32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
    32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
    32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// Student to add rest of the code here
sha_256_processor proc1(.clk(clk), .start(proc1_e), .rstn(proc1_rstn), .count(j), .h0(h0_1), .h1(h1_1), .h2(h2_1), .h3(h3_1), .h4(h4_1),
.h5(h5_1), .h6(h6_1), .h7(h7_1), .w0(w[0]), .w1(w[1]), .w2(w[2]), .w3(w[3]), .w4(w[4]), .w5(w[5]), .w6(w[6]), .w7(w[7]), .w8(w[8]),
.w9(w[9]), .w10(w[10]), .w11(w[11]), .w12(w[12]), .w13(w[13]), .w14(w[14]), .w15(w[15]), .out_h0(phase1_out0), .out_h1(phase1_out1), .out_h2(phase1_out2),
.out_h3(phase1_out3), .out_h4(phase1_out4), .out_h5(phase1_out5), .out_h6(phase1_out6), .out_h7(phase1_out7), .done(proc1_done)); 

sha_256_processor proc2(.clk(clk), .start(proc2_e), .rstn(proc2_rstn), .count(j), .h0(h0_2), .h1(h1_2), .h2(h2_2), .h3(h3_2), .h4(h4_2),
.h5(h5_2), .h6(h6_2), .h7(h7_2), .w0(w[16]), .w1(w[17]), .w2(w[18]), .w3(nonce), .w4(one_word), .w5(zero_word[0]), .w6(zero_word[1]), .w7(zero_word[2]),
.w8(zero_word[3]), .w9(zero_word[4]), .w10(zero_word[5]), .w11(zero_word[6]), .w12(zero_word[7]), .w13(zero_word[8]), .w14(zero_word[9]),
.w15(32'd640), .out_h0(phase2_out0), .out_h1(phase2_out1), .out_h2(phase2_out2),
.out_h3(phase2_out3), .out_h4(phase2_out4), .out_h5(phase2_out5), .out_h6(phase2_out6), .out_h7(phase2_out7), .done(proc2_done)); 

sha_256_processor proc3(.clk(clk), .start(proc3_e), .rstn(proc3_rstn), .count(j), .h0(h0_3), .h1(h1_3), .h2(h2_3), .h3(h3_3), .h4(h4_3),
.h5(h5_3), .h6(h6_3), .h7(h7_3), .w0(phase2_out0), .w1(phase2_out1), .w2(phase2_out2), .w3(phase2_out3), .w4(phase2_out4), .w5(phase2_out5), .w6(phase2_out6),
.w7(phase2_out7), .w8(one_word), .w9(zero_word), .w10(zero_word), .w11(zero_word), .w12(zero_word), .w13(zero_word), .w14(zero_word),
.w15(32'd256), .out_h0(phase3_out0), .out_h1(phase3_out1), .out_h2(phase3_out2),
.out_h3(phase3_out3), .out_h4(phase3_out4), .out_h5(phase3_out5), .out_h6(phase3_out6), .out_h7(phase3_out7), .done(proc3_done)); 


assign nonce = j;

always_ff @(posedge clk)
begin
	case (state)
	IDLE:begin
		proc1_rstn <= 0;
		proc2_rstn <= 0;
		proc3_rstn <= 0;
		
		proc1_e <= 0;
		proc2_e <= 0;
		proc3_e <= 0;
		
		//nonce <= 32'b0;
		zero_word <= 32'b0;
		one_word <= 31'b1;
		if (start)
		begin
				i <= 0;
				j <= 0;
				offset <= 0;
				cur_we <= 0;
				cur_addr <= message_addr;
				cur_write_data <= 32'h0;
				state <= READ;
				proc1_rstn <= 1;
				proc2_rstn <= 1;
				proc3_rstn <= 1;
				
				proc1_e <= 0;
				proc2_e <= 0;
				proc3_e <= 0;
				
				h0_1 <= 32'h6a09e667;
				h1_1 <= 32'hbb67ae85;
				h2_1 <= 32'h3c6ef372;
				h3_1 <= 32'ha54ff53a;
				h4_1 <= 32'h510e527f;
				h5_1 <= 32'h9b05688c;
				h6_1 <= 32'h1f83d9ab;
				h7_1 <= 32'h5be0cd19;
		end
	end
	
	READ:begin
		if (i < 20)
			begin
			if (i >= 1)
				begin
				w[i-1] <= mem_read_data;
				end
			i <= i+1;
			offset <= offset+1;
			state <= READ;
		end
		else
		begin
			//proc1enable
			proc1_e <= 1;
		
			if (proc1_done == 1)
			begin
				proc1_e <= 0;
				proc1_rstn <= 1;
				proc2_e <= 1;
		
				state <= PROCESS;
			end
		end		
		
		//store proc1 data
	end
	
	PROCESS:begin
		if (j < 16)
		begin
		if (proc2_done == 1)
			begin
				proc2_e <= 0;
				proc3_e <= 1;
				state <= PROCESS;
			end
		else if (proc3_done == 1)
			begin
				proc3_e <= 0;
				final_out[j] <= phase3_out0;
			end
		else
			begin
				j <= j+1;
				state <= PROCESS;
			end
		end
		
		else 
		begin
			state <= WRITE;
		end
		
		
	end
	
	WRITE:begin
	
	end
	
	default: state <= IDLE;
	endcase
	
end






endmodule
