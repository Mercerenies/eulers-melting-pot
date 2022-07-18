module problem136;

   parameter LIMIT = 50000000;

   reg [63:0] solutions [LIMIT-1:0];
   reg [63:0] x;
   reg [63:0] m;
   reg [63:0] n;
   reg [63:0] i;
   reg [63:0] total;
   initial begin

      for (i = 0; i < LIMIT; i++) begin
         solutions[i[25:0]] = 0;
      end

      for (x = 1; x < LIMIT; x++) begin
         for (m = x / 3 + 1; m <= LIMIT; m++) begin
            n = (3 * m - x) * (x + m);
            if (n >= LIMIT) begin
               break;
            end
            if (n > 0) begin
               solutions[n[25:0]]++;
            end
         end
      end // for (x = 1; x < LIMIT; x++)

      total = 0;
      for (i = 1; i < LIMIT; i++) begin
         if (solutions[i[25:0]] == 1) begin
            total += 1;
         end
      end
      $display(total);
      $finish;

   end

endmodule
