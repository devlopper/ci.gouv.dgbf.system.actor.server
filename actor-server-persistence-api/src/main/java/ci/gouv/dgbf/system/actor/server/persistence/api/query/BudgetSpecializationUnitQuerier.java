package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;

public interface BudgetSpecializationUnitQuerier extends Querier.CodableAndNamable<BudgetSpecializationUnit> {

	//String QUERY_IDENTIFIER_READ_BY_IDENTIFIERS = QueryIdentifierGetter.getInstance().get(BudgetSpecializationUnit.class, QueryName.READ_BY_SYSTEM_IDENTIFIERS);
	//String QUERY_IDENTIFIER_COUNT_BY_IDENTIFIERS = QueryIdentifierGetter.getInstance().getCountFromRead(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIERS);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetSpecializationUnit> implements BudgetSpecializationUnitQuerier,Serializable {
		
		@Override
		protected Class<BudgetSpecializationUnit> getKlass() {
			return BudgetSpecializationUnit.class;
		}
		
	}
	
	/**/
	
	static BudgetSpecializationUnitQuerier getInstance() {
		return Helper.getInstance(BudgetSpecializationUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(BudgetSpecializationUnit.class);
		
	}
}