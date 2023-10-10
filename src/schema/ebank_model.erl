-module(ebank_model).

-callback schema() -> ebank_schema:t().
