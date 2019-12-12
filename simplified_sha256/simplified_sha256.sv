module simplified_sha256(
 input logic  clk, reset_n, start,
 input logic  [15:0] message_addr, output_addr,
 output logic done, mem_clk, mem_we,
 output logic [15:0] mem_addr,
 output logic [31:0] mem_write_data,
 input logic [31:0] mem_read_data);

// FSM state variables 
enum logic [1:0] {IDLE, BLOCK, COMPUTE, WRITE} state;

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[17];
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] dig_0, dig_1, dig_2, dig_3, dig_4, dig_5, dig_6, dig_7;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;

logic proc_e;
logic proc_rstn;
logic proc_done;

parameter size = 20; // hard-code it to 20 words
assign num_blocks = determine_num_blocks(size); // assume no more than 256 blocks = 16,384 bytes


// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  return (size + 16 - 1) / 16;

endfunction

// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign mem_clk = clk;
assign mem_addr = cur_addr + offset;
assign mem_we = cur_we;
assign mem_write_data = cur_write_data;

sha_256_processor proc(.clk(clk), .start(proc_e), .rstn(proc_rstn), .count(j), .h0(h0), .h1(h1), .h2(h2), .h3(h3), .h4(h4),
.h5(h5), .h6(h6), .h7(h7), .w0(w[0]), .w1(w[1]), .w2(w[2]), .w3(w[3]), .w4(w[4]), .w5(w[5]), .w6(w[6]), .w7(w[7]), .w8(w[8]),
.w9(w[9]), .w10(w[10]), .w11(w[11]), .w12(w[12]), .w13(w[13]), .w14(w[14]), .w15(w[15]), .out_h0(dig_0), .out_h1(dig_1), .out_h2(dig_2),
.out_h3(dig_3), .out_h4(dig_4), .out_h5(dig_5), .out_h6(dig_6), .out_h7(dig_7), .done(proc_done)); 

// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
	 proc_rstn <= 0;
	 proc_e <= 0;
	 
  end 
  else case (state)
    IDLE: begin 
       if(start) begin
			 //block gathering counter
			 i <= 0;
			 
          // Number of block iteration variable
          j <= 0;
			 
          // initialize pointer to access memory location
          offset <= 0;
	 
			 // by default set write enable to '0' (i.e. memory read mode)
          cur_we <= 1'b0;
			 
			// get starting address of message 
          cur_addr <= message_addr;
			 
			// initialize write data to memory to '0'
          cur_write_data <= 32'h0;
			 
			// proceed to message block fetch stage
          state <= BLOCK;
			 
			 proc_rstn <= 1;
        end
    end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	   if (j >= num_blocks)
		begin
			//jump to write
			i <= 0;
			offset <= 0;
			cur_we <= 1;
			state <= WRITE;
		end
		
		else if (j == 0 && i < 16)
		begin
			if (i >= 1)
				begin
				w[i-1] <= mem_read_data;
				end
			i <= i+1;
			offset <= offset+1;
			state <= BLOCK;
		end
		
		else if (j == 1 && i < 4)
		begin
			if (i >= 1)
				begin
				w[i-1] <= mem_read_data;
				end
			i <= i+1;
			offset <= offset+1;
			state <= BLOCK;
			w[4] = 32'h80000000;
			w[5] = 0;
			w[6] = 0;
			w[7] = 0;
			w[8] = 0;
			w[9] = 0;
			w[10] = 0;
			w[11] = 0;
			w[12] = 0;
			w[13] = 0;
			w[14] = 0;
			w[15] = 32'h00000280;
		end
		
		else
		begin
			w[i-1] <= mem_read_data;
			i <= 0;
			state <= COMPUTE;  
		end
	 end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
		if (j == 0)
		begin
			//Initialize hash values:
			h0 <= 32'h6a09e667;
			h1 <= 32'hbb67ae85;
			h2 <= 32'h3c6ef372;
			h3 <= 32'ha54ff53a;
			h4 <= 32'h510e527f;
			h5 <= 32'h9b05688c;
			h6 <= 32'h1f83d9ab;
			h7 <= 32'h5be0cd19;
		end
		else
		begin
			h0 <= dig_0;
			h1 <= dig_1;
			h2 <= dig_2;
			h3 <= dig_3;
			h4 <= dig_4;
			h5 <= dig_5;
			h6 <= dig_6;
			h7 <= dig_7;
		end
		proc_e <= 1;
		
		if (proc_done == 1)
		begin
			proc_e <= 0;
			state <= BLOCK;
			j <= j+1;
		end
		else
		begin
			state <= COMPUTE;
		end
		
    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
		//write to output memory
		cur_we = 1;
		if (i < 8)
		begin
			case (i)
			0:	cur_write_data <= dig_0;
			1: cur_write_data <= dig_1;
			2: cur_write_data <= dig_2;
			3: cur_write_data <= dig_3;
			4: cur_write_data <= dig_4;
			5: cur_write_data <= dig_5;
			6: cur_write_data <= dig_6; 
			7: cur_write_data <= dig_7;
			default: cur_write_data <= dig_0;
			endcase
			offset <= offset + 1;
			i <= i + 1;
		end
    end
   endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
