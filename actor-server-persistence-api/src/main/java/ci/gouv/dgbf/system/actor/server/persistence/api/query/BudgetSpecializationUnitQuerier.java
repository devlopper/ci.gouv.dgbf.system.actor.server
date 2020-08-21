package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.value.Value;

public interface BudgetSpecializationUnitQuerier extends Querier {

	//String QUERY_IDENTIFIER_READ_BY_IDENTIFIERS = QueryIdentifierGetter.getInstance().get(BudgetSpecializationUnit.class, QueryName.READ_BY_SYSTEM_IDENTIFIERS);
	//String QUERY_IDENTIFIER_COUNT_BY_IDENTIFIERS = QueryIdentifierGetter.getInstance().getCountFromRead(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIERS);
	
	/**/
	
	static BudgetSpecializationUnitQuerier getInstance() {
		return Helper.getInstance(BudgetSpecializationUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
}
