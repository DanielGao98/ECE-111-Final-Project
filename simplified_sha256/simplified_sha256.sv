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
logic [31:0] w[16];
logic [31:0] h0, h1, h2, h3, h4, h5, h6, h7;
logic [31:0] a, b, c, d, e, f, g, h;
logic [ 7:0] i, j;
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        cur_we;
logic [15:0] cur_addr;
logic [31:0] cur_write_data;

logic proc_e;
logic proc_rstn;

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

sha_256_processor proc(.clk(clk), .start(proc_e), .rstn(proc_rstn), .count(j), .w0(w[0]), .w1(w[1]), 

// SHA-256 FSM 
// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
// and write back hash value back to memory
always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    cur_we <= 1'b0;
    state <= IDLE;
  end 
  else case (state)
    IDLE: begin 
       if(start) begin
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
        end
    end

    // SHA-256 FSM 
    // Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
    // and write back hash value back to memory
    BLOCK: begin
	 ` if (j >= num_blocks)
		begin
			state <= WRITE;
		end
		
		else if (i <= 16)
		begin
			w[i] <= mem_read_data;
			i <= i+1;
			offset <= offset+1;
		end
		else
		begin
			state <= COMPUTE;  
		end
	 end

    // For each block compute hash function
    // Go back to BLOCK stage after each block hash computation is completed and if
    // there are still number of message blocks available in memory otherwise
    // move to WRITE stage
    COMPUTE: begin
		proc_e <= 1;
		
		if (proc_done == 1)
		begin
			state <= BLOCK;
		end
		
    end

    // h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
    // h0 to h7 after compute stage has final computed hash value
    // write back these h0 to h7 to memory starting from output_addr
    WRITE: begin
		//write to output memory
		curr_we = 1;
		offset = 0;
		for (int i = 0; i < 16; i+=1)begin
			mem_write_data = h0
		end
   


    end
   endcase
  end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
