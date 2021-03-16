package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.EconomicNature;

public interface EconomicNatureQuerier extends Querier.CodableAndNamable<EconomicNature> {

	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(EconomicNature.class, "readAllForUI");
	Collection<EconomicNature> readAllForUI();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<EconomicNature> implements EconomicNatureQuerier,Serializable {
	
		@Override
		public Collection<EconomicNature> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Collection<EconomicNature> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(EconomicNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		protected Class<EconomicNature> getKlass() {
			return EconomicNature.class;
		}
	}
	
	/**/
	
	static EconomicNatureQuerier getInstance() {
		return Helper.getInstance(EconomicNatureQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(EconomicNature.class);
		QueryManager.getInstance().register(Query.buildSelect(EconomicNature.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM EconomicNature t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(EconomicNature.FIELD_IDENTIFIER,EconomicNature.FIELD_CODE,EconomicNature.FIELD_NAME));
	}
}