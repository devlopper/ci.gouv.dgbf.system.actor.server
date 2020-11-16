package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribut;

public interface IdentificationFormAttributQuerier extends Querier {

	String PARAMETER_NAME_FORM = "form";
	String PARAMETER_NAME_FORM_CODE = "formCode";
	
	String QUERY_IDENTIFIER_READ_BY_FORM_CODE = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribut.class, "readByFormCode");
	Collection<IdentificationFormAttribut> readByFormCode(String formCode);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements IdentificationFormAttributQuerier,Serializable {
		
		@Override
		public Collection<IdentificationFormAttribut> readByFormCode(String formCode) {
			return QueryExecutor.getInstance().executeReadMany(IdentificationFormAttribut.class, QUERY_IDENTIFIER_READ_BY_FORM_CODE, PARAMETER_NAME_FORM_CODE,formCode);
		}

	}
	
	/**/
	
	static IdentificationFormAttributQuerier getInstance() {
		return Helper.getInstance(IdentificationFormAttributQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.buildSelect(IdentificationFormAttribut.class, QUERY_IDENTIFIER_READ_BY_FORM_CODE, "SELECT t FROM IdentificationFormAttribut t WHERE t.form.code = :"
					+PARAMETER_NAME_FORM_CODE+" ORDER BY t.orderNumber ASC,t.identifier ASC")
		);
	}
}