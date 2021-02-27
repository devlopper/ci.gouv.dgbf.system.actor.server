package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

public interface LocalityQuerier extends Querier.CodableAndNamable<Locality> {

	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(Locality.class, "readByCode");
	Locality readByCode(String code);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Locality> implements LocalityQuerier,Serializable {
		
		@Override
		public Locality readByCode(String code) {
			return QueryExecutor.getInstance().executeReadOne(Locality.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_CODE)
					.addFilterFieldsValues(PARAMETER_NAME_CODE,code));
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
		QueryHelper.addQueries(
				Query.buildSelect(Locality.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM Locality t WHERE t.code = :"+PARAMETER_NAME_CODE)
			);
	}
}