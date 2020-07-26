package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;

public interface BudgetaryFunctionQuerier extends Querier.CodableAndNamable<BudgetaryFunction> {
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetaryFunction> implements BudgetaryFunctionQuerier,Serializable {
		@Override
		protected Class<BudgetaryFunction> getKlass() {
			return BudgetaryFunction.class;
		}
	}
	
	/**/
	
	static BudgetaryFunctionQuerier getInstance() {
		return Helper.getInstance(BudgetaryFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(BudgetaryFunction.class);
	}
}