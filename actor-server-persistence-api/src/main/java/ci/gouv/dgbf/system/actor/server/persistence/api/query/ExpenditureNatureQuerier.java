package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;

public interface ExpenditureNatureQuerier extends Querier.CodableAndNamable<ExpenditureNature> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(ExpenditureNature.class, "readAllForUI");
	Collection<ExpenditureNature> readAllForUI();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ExpenditureNature> implements ExpenditureNatureQuerier,Serializable {
	
		@Override
		public Collection<ExpenditureNature> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Collection<ExpenditureNature> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(ExpenditureNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		protected Class<ExpenditureNature> getKlass() {
			return ExpenditureNature.class;
		}
	}
	
	/**/
	
	static ExpenditureNatureQuerier getInstance() {
		return Helper.getInstance(ExpenditureNatureQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ExpenditureNature.class);
		QueryHelper.addQueries(Query.buildSelect(ExpenditureNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM ExpenditureNature t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ExpenditureNature.FIELD_IDENTIFIER,ExpenditureNature.FIELD_CODE,ExpenditureNature.FIELD_NAME));
	}
}