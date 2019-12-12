module sha_256_processor(
	input logic clk,
	input logic start,
	input logic rstn,
	input logic [1:0] count,
	
	input logic[31:0] h0,
	input logic[31:0] h1,
	input logic[31:0] h2,
	input logic[31:0] h3,
	input logic[31:0] h4,
	input logic[31:0] h5,
	input logic[31:0] h6,
	input logic[31:0] h7,
	
	input logic[31:0] w0,
	input logic[31:0] w1,
	input logic[31:0] w2,
	input logic[31:0] w3,
	input logic[31:0] w4,
	input logic[31:0] w5,
	input logic[31:0] w6,
	input logic[31:0] w7,
	input logic[31:0] w8,
	input logic[31:0] w9,
	input logic[31:0] w10,
	input logic[31:0] w11,
	input logic[31:0] w12,
	input logic[31:0] w13,
	input logic[31:0] w14,
	input logic[31:0] w15,
	
	output logic[31:0] out_h0,
	output logic[31:0] out_h1,
	output logic[31:0] out_h2,
	output logic[31:0] out_h3,
	output logic[31:0] out_h4,
	output logic[31:0] out_h5,
	output logic[31:0] out_h6,
	output logic[31:0] out_h7,
	
	output logic done
);

enum logic [1:0] {IDLE, READ, PROCESS, WRITE} state;
logic [31:0] a, b, c, d, e, f, g, h;
logic [255:0] op_ret;
logic [31:0] w [65];
logic [31:0] s0, s1;
logic [31:0] i;

// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};

// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w, k);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = rightrotate(e, 6) ^ rightrotate(e, 11) ^ rightrotate(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = ch + S1 + h + k + w;
    S0 = rightrotate(a, 2) ^ rightrotate(a, 13) ^ rightrotate(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;
    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

function logic [31:0] rightrotate(input logic [31:0] x,
                                  input logic [ 7:0] r);
   // Student to add function implementation
	rightrotate = ((x >> r) | (x << (32-r)));
endfunction


always_ff @(posedge clk, negedge rstn)
begin
	if (!rstn)
	begin
		state <= IDLE;
		
	end
	else case (state)
	IDLE:begin
		done = 0;
		//out_h0 = 0;
		//out_h1 = 0;
		//out_h2 = 0;
		//out_h3 = 0;
		//out_h4 = 0;
		//out_h5 = 0;
		//out_h6 = 0;
		//out_h7 = 0;
		
		//a = 0;
		//b = 0;
		//c = 0; 
		//d = 0;
		//e = 0;
		//f = 0;
		//g = 0;
		//h = 0;
		
		i = 0;
		
		done = 0;
		
		if (start)
		begin
			state = READ;
		end
		else
		begin
			state = IDLE;
		end
	end
	
	READ:begin
		a = h0;
		b = h1;
		c = h2;
		d = h3;
		e = h4;
		f = h5;
		g = h6;
		h = h7;
		
		w[0] = w0;
		w[1] = w1;
		w[2] = w2;
		w[3] = w3;
		w[4] = w4;
		w[5] = w5;
		w[6] = w6;
		w[7] = w7;
		w[8] = w8;
		w[9] = w9;
		w[10] = w10;
		w[11] = w11;
		w[12] = w12;
		w[13] = w13;
		w[14] = w14;
		w[15] = w15;
		
		if (start)
		begin
			state = PROCESS;
		end
		else
		begin
			state = IDLE;
		end
	end
	
	PROCESS:begin
		//65 cycles
		if (i < 64)
		begin
		
			if (i > 15) 
			begin
				s0 = rightrotate(w[i - 15], 7) ^ rightrotate(w[i-15], 18) ^ (w[i-15] >> 3);
				s1 = rightrotate(w[i - 2], 17) ^ rightrotate(w[i-2], 19) ^ (w[i-2] >> 10);
				w[i] = w[i-16] + s0 + w[i-7] + s1;
			end
			
			
			{a,b,c,d,e,f,g,h} = sha256_op(a, b, c, d, e, f, g, h, w[i], k[i]);
			
			//h = op_ret[31:0];
			//g = op_ret[63:32];
			//f = op_ret[95:64];
			//e = op_ret[127:96];
			//d = op_ret[159:128];
			//c = op_ret[191:160];
			//b = op_ret[223:192];
			//a = op_ret[255:224];
			
			i = i + 1;
			state = PROCESS;		
			
		end
		
		else
		begin
			state = WRITE;
			//done = 1;
		end
		
		
	end
	
	WRITE:begin
		out_h0 = h0+a;
		out_h1 = h1+b;
		out_h2 = h2+c;
		out_h3 = h3+d;
		out_h4 = h4+e;
		out_h5 = h5+f;
		out_h6 = h6+g;
		out_h7 = h7+h;
		
		done = 1;
		i = 0;
		state = IDLE;
		
	end
	endcase
end

endmodule
