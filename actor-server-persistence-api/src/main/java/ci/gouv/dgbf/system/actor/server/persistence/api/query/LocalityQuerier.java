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

import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

public interface LocalityQuerier extends Querier.CodableAndNamable<Locality> {

	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(Locality.class, "readByCode");
	Locality readByCode(String code);
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Locality.class, QueryName.READ_DYNAMIC);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Locality> implements LocalityQuerier,Serializable {
		
		@Override
		public Locality readByCode(String code) {
			return QueryExecutor.getInstance().executeReadOne(Locality.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_CODE)
					.addFilterFieldsValues(PARAMETER_NAME_CODE,code));
		}
		
		@Override
		public Collection<Locality> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Locality.class,arguments.setQuery(null));
			return super.readMany(arguments);
		}
		
		@Override
		protected Class<Locality> getKlass() {
			return Locality.class;
		}
	}
	
	/**/
	
	static LocalityQuerier getInstance() {
		return Helper.getInstance(LocalityQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Locality.class);
		QueryManager.getInstance().register(
				Query.buildSelect(Locality.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM Locality t WHERE t.code = :"+PARAMETER_NAME_CODE)
			);
	}
}