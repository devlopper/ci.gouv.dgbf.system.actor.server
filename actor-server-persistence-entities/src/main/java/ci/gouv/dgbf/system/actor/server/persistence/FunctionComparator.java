package ci.gouv.dgbf.system.actor.server.persistence;

import java.util.Comparator;
import java.util.Map;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;

public class FunctionComparator implements Comparator<Function> {

	@Override
	public int compare(Function function1, Function function2) {
		return ORDERS_INDEXES.get(function1.getCode()).compareTo(ORDERS_INDEXES.get(function2.getCode()));
	}
	
	public static final Map<String,Integer> ORDERS_INDEXES = Map.of(
			Function.CODE_CREDIT_MANAGER_HOLDER,0
			,Function.CODE_CREDIT_MANAGER_ASSISTANT,1
			,Function.CODE_AUTHORIZING_OFFICER_HOLDER,2
			,Function.CODE_AUTHORIZING_OFFICER_ASSISTANT,3
			,Function.CODE_FINANCIAL_CONTROLLER_HOLDER,4
			,Function.CODE_FINANCIAL_CONTROLLER_ASSISTANT,5
			,Function.CODE_ACCOUNTING_HOLDER,6
			,Function.CODE_ACCOUNTING_ASSISTANT,7
		);
}