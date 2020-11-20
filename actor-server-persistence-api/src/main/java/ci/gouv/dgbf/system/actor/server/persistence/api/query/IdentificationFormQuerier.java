package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribut;

public interface IdentificationFormQuerier extends Querier {

	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(IdentificationForm.class, "readByCode");
	IdentificationForm readByCode(String code);
	
	IdentificationForm readWithFieldsByCode(String code);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements IdentificationFormQuerier,Serializable {
		@Override
		public IdentificationForm readByCode(String code) {
			return QueryExecutor.getInstance().executeReadOne(IdentificationForm.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_CODE).addFilterField(PARAMETER_NAME_CODE, code));
		}
		
		@Override
		public IdentificationForm readWithFieldsByCode(String code) {
			IdentificationForm form = readByCode(code);
			if(form == null)
				return null;
			setFields(form, null);
			return form;
		}
		
		public static void setFields(IdentificationForm form,Boolean asString) {
			Collection<IdentificationFormAttribut> formAttributs = IdentificationFormAttributQuerier.getInstance().readByFormCode(form.getCode());
			if(CollectionHelper.isEmpty(formAttributs))
				return;
			if(Boolean.TRUE.equals(asString)) {
				
			}else {
				form.setAttributs(formAttributs.stream().map(x -> x.getAttribut().setRequired(x.getRequired() == null ? x.getAttribut().getRequired() : x.getRequired())
						.setOrderNumber(x.getOrderNumber())).collect(Collectors.toList()));
			}			
		}
	}
	
	/**/
	
	static IdentificationFormQuerier getInstance() {
		return Helper.getInstance(IdentificationFormQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
				Query.buildSelect(IdentificationForm.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM IdentificationForm t WHERE t.code = :"+PARAMETER_NAME_CODE)
		);
	}
}