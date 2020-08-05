package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetaryFunction;

public interface BudgetaryFunctionQuerier extends Querier.CodableAndNamable<BudgetaryFunction> {
	
	String PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER = "accountRequestIdentifier";
	
	/*read by account request identifier*/
	String QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER = Querier.buildIdentifier(BudgetaryFunction.class,"readByAccountRequestIdentifier");
	Collection<BudgetaryFunction> readByAccountRequestIdentifier(String accountRequestIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetaryFunction> implements BudgetaryFunctionQuerier,Serializable {
		@Override
		protected Class<BudgetaryFunction> getKlass() {
			return BudgetaryFunction.class;
		}
		
		@Override
		public Collection<BudgetaryFunction> readByAccountRequestIdentifier(String accountRequestIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(BudgetaryFunction.class,QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER, PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER, accountRequestIdentifier);
		}
		
		@Override
		public Collection<BudgetaryFunction> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readByAccountRequestIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER));
			return super.readMany(arguments);
		}
	}
	
	/**/
	
	static BudgetaryFunctionQuerier getInstance() {
		return Helper.getInstance(BudgetaryFunctionQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(BudgetaryFunction.class);
		QueryHelper.addQueries(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACCOUNT_REQUEST_IDENTIFIER
				,Query.FIELD_TUPLE_CLASS,BudgetaryFunction.class,Query.FIELD_RESULT_CLASS,BudgetaryFunction.class
				,Query.FIELD_VALUE,Language.of(Select.of("t")
						,From.of("BudgetaryFunction t","INNER JOIN AccountRequestBudgetaryFunction f ON f.budgetaryFunction = t")
						,Where.of("f.accountRequest.identifier = :"+PARAMETER_NAME_ACCOUNT_REQUEST_IDENTIFIER))
				)
			);
	}
}