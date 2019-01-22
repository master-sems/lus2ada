-- Here starts generated code by David Guerin (RGF2aWQ=)
procedure STABLE is
  init_0: Boolean := true;
  -- INPUTS (PRE)
  -- VARS
  count: integer;
  -- VARS (PRE)
  pre_count: integer;
  -- OUTPUTS (PRE)
  pre_level: boolean;
  procedure step 
    (set: boolean; delay0: integer; level: out boolean) is
  begin
  level := (count > 0);
  count := (if set then delay0 else (if (if init_0 then false else (pre_level)) then ((pre_count) - 1) else 0));
  pre_level := level;
  pre_count := count;
  init_0 := false;
  end;
begin 
  null;
end STABLE;
-- Here ends generated code by David Guerin
