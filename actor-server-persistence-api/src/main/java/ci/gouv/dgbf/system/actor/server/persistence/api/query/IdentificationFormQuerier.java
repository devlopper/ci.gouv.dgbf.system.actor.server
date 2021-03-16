package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.query.TypedQuerier;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;

public interface IdentificationFormQuerier extends TypedQuerier<IdentificationForm,String> {

	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(IdentificationForm.class, "readByCode");
	IdentificationForm readByCode(String code);
	
	IdentificationForm readWithFieldsByCode(String code);
	
	String QUERY_IDENTIFIER_READ_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationForm.class, QueryName.READ_FOR_UI.getValue());	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationForm.class, QueryName.READ_BY_IDENTIFIER_FOR_UI.getValue());
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(IdentificationForm.class, QueryName.READ_BY_IDENTIFIER_FOR_EDIT.getValue());
	
	/**/
	
	public static abstract class AbstractImpl extends TypedQuerier.AbstractImpl<IdentificationForm,String> implements IdentificationFormQuerier,Serializable {
		
		@Override
		public Class<IdentificationForm> getType() {
			return IdentificationForm.class;
		}
		
		@Override
		protected IdentificationForm __readOne__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.__readOne__(arguments);
		}
		
		@Override
		protected Collection<IdentificationForm> __readMany__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_FOR_UI))
				return readForUI(arguments);
			return super.__readMany__(arguments);
		}
		
		@Override
		protected Long __count__(QueryExecutorArguments arguments) {
			return super.__count__(arguments);
		}
				
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
			Collection<IdentificationFormAttribute> formAttributes = IdentificationFormAttributeQuerier.getInstance().readByFormCode(form.getCode());
			if(CollectionHelper.isEmpty(formAttributes))
				return;
			if(Boolean.TRUE.equals(asString)) {
				
			}else {
				form.setAttributs(formAttributes.stream()
						.map(x -> x.getAttribute()
							.setName(StringHelper.isBlank(x.getName()) ? x.getAttribute().getName() : x.getName())
							.setRequired(x.getRequired() == null ? x.getAttribute().getRequired() : x.getRequired())
							.setOrderNumber(x.getOrderNumber()))
						.collect(Collectors.toList())
						);
			}			
		}
	}
	
	/**/
	
	static IdentificationFormQuerier getInstance() {
		return Helper.getInstance(IdentificationFormQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		TypedQuerier.initialize(IdentificationForm.class);
		QueryManager.getInstance().register(
			Query.buildSelect(IdentificationForm.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM IdentificationForm t WHERE t.code = :"+PARAMETER_NAME_CODE)
			,Query.buildSelect(IdentificationForm.class, QUERY_IDENTIFIER_READ_FOR_UI, "SELECT t.identifier,t.code,t.name FROM IdentificationForm t ORDER BY t.code ASC")
			.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationForm.FIELD_IDENTIFIER,IdentificationForm.FIELD_CODE,IdentificationForm.FIELD_NAME)
			,Query.buildSelect(IdentificationForm.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
					, "SELECT t FROM IdentificationForm t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			,Query.buildSelect(IdentificationForm.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
					, "SELECT t FROM IdentificationForm t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
		);
	}
}