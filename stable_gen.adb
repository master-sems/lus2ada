-- Here starts generated code by David Guerin
procedure STABLE is
  -- INPUTS (PRE)

  -- VARS
  count: integer;

  -- VARS (PRE)
  pre_count: integer;

  -- OUTPUTS (PRE)
  pre_count: integer;

  procedure step is
    (set: boolean; delay0: integer; level: out boolean;)
  begin
  level := (count > 0);
  count := (if set then delay0 else (if (if false then false else (pre_level)) then ((pre_count) - 1) else 0));
  end
begin 
  null;
end STABLE;
-- Here ends generated code by David Guerin
