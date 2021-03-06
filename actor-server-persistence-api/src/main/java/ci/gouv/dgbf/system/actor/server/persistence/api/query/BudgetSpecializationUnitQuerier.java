package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = BudgetSpecializationUnit.class,name = BudgetSpecializationUnitQuerier.QUERY_NAME_READ
				,value = "SELECT t FROM BudgetSpecializationUnit t ORDER BY t.code ASC")
	})
public interface BudgetSpecializationUnitQuerier extends Querier.CodableAndNamable<BudgetSpecializationUnit> {

	/* read order by code ascending */
	String QUERY_NAME_READ = "read";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, QUERY_NAME_READ);
	Collection<BudgetSpecializationUnit> read();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetSpecializationUnit> implements BudgetSpecializationUnitQuerier,Serializable {
		
		@Override
		public Collection<BudgetSpecializationUnit> read() {
			return QueryExecutor.getInstance().executeReadMany(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
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