package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Country;

public interface CountryQuerier extends Querier.CodableAndNamable<Country> {

	String PARAMETER_NAME_SEARCH = "search";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Country.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Country.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Country.class, QueryName.COUNT_DYNAMIC);
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ALL = "read";
	String QUERY_IDENTIFIER_READ_ALL = QueryIdentifierBuilder.getInstance().build(Country.class, QUERY_NAME_READ_ALL);
	Collection<Country> read();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(Country.class, "readAllForUI");
	Collection<Country> readAllForUI();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_ALL);
	Long count();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Country> implements CountryQuerier,Serializable {
		@Override
		public Collection<Country> read() {
			return QueryExecutor.getInstance().executeReadMany(Country.class, QUERY_IDENTIFIER_READ_ALL);
		}
		
		@Override
		public Collection<Country> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(Country.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Country readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(Country.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Country> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Country.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_READ_ALL.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(Country.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		protected Class<Country> getKlass() {
			return Country.class;
		}
	}
	
	/**/
	
	static CountryQuerier getInstance() {
		return Helper.getInstance(CountryQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Country.class);
		QueryManager.getInstance().register(
				Query.buildSelect(Country.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI, "SELECT t.identifier,t.code,t.name FROM Country t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(Country.FIELD_IDENTIFIER,Country.FIELD_CODE,Country.FIELD_NAME)
				
		);		
	}
}