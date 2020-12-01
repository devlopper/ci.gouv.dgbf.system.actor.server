package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.kase;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.whens;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.whenEqual;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.QueryType;
import org.cyk.utility.__kernel__.persistence.query.TypedQuerier;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;

public interface IdentificationFormAttributeQuerier extends TypedQuerier<IdentificationFormAttribute,String> {

	String PARAMETER_NAME_FORM = "form";
	String PARAMETER_NAME_FORM_CODE = "formCode";
	String PARAMETER_NAME_FORM_IDENTIFIER = "formIdentifier";
	String PARAMETER_NAME_FORM_IDENTIFIER_NULLABLE = PARAMETER_NAME_FORM_IDENTIFIER+"Nullable";
	
	String QUERY_IDENTIFIER_READ_BY_FORM_CODE = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribute.class, "readByFormCode");
	Collection<IdentificationFormAttribute> readByFormCode(String formCode);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER.getValue());
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribute.class, QueryName.COUNT_WHERE_FILTER.getValue());
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_UI.getValue());
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(IdentificationFormAttribute.class, QueryName.READ_WHERE_FILTER_FOR_EDIT.getValue());
	
	/**/
	
	public static abstract class AbstractImpl extends TypedQuerier.AbstractImpl<IdentificationFormAttribute,String> implements IdentificationFormAttributeQuerier,Serializable {
		
		@Override
		public Class<IdentificationFormAttribute> getType() {
			return IdentificationFormAttribute.class;
		}
		
		@Override
		protected void prepareReadWhereFilter(QueryExecutorArguments arguments, Filter filter) {
			super.prepareReadWhereFilter(arguments, filter);
			filter.addFieldsNullable(arguments, PARAMETER_NAME_FORM_IDENTIFIER);
			filter.addFieldEquals(PARAMETER_NAME_FORM_IDENTIFIER, arguments);
		}
		
		@Override
		public Collection<IdentificationFormAttribute> readByFormCode(String formCode) {
			return QueryExecutor.getInstance().executeReadMany(IdentificationFormAttribute.class, QUERY_IDENTIFIER_READ_BY_FORM_CODE, PARAMETER_NAME_FORM_CODE,formCode);
		}
	}
	
	/**/
	
	static IdentificationFormAttributeQuerier getInstance() {
		return Helper.getInstance(IdentificationFormAttributeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		TypedQuerier.initialize(IdentificationFormAttribute.class);
		QueryHelper.addQueries(
			Query.buildSelect(IdentificationFormAttribute.class, QUERY_IDENTIFIER_READ_BY_FORM_CODE, "SELECT t FROM IdentificationFormAttribute t WHERE t.form.code = :"
					+PARAMETER_NAME_FORM_CODE+" ORDER BY t.orderNumber ASC,t.identifier ASC")
			,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql("SELECT COUNT(t.identifier)",getReadWhereFilterFrom(),getReadWhereFilterWhere()))
			
			,Query.buildSelect(IdentificationFormAttribute.class, QUERY_IDENTIFIER_READ_WHERE_FILTER
					, jpql(getReadWhereFilterSelect(),getReadWhereFilterFrom(),getReadWhereFilterWhere(),getReadWhereFilterOrder()))
				.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER
						,IdentificationFormAttribute.FIELD_REQUIRED,IdentificationFormAttribute.FIELD_FORM
						,IdentificationFormAttribute.FIELD_ATTRIBUTE).setType(QueryType.READ_MANY)
			
			,Query.buildSelect(IdentificationFormAttribute.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
					, jpql(getReadWhereFilterForUISelect(),getReadWhereFilterFrom(),getReadWhereFilterWhere(),getReadWhereFilterOrder()))
				.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER
						,IdentificationFormAttribute.FIELD_REQUIRED_AS_STRING,IdentificationFormAttribute.FIELD_FORM_AS_STRING
						,IdentificationFormAttribute.FIELD_ATTRIBUTE_AS_STRING).setType(QueryType.READ_MANY)
				
			,Query.buildSelect(IdentificationFormAttribute.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_EDIT
					, jpql(getReadWhereFilterForEditSelect(),getReadWhereFilterFrom(),getReadWhereFilterWhere(),getReadWhereFilterOrder()))
				.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER
						,IdentificationFormAttribute.FIELD_REQUIRED,IdentificationFormAttribute.FIELD_FORM
						,IdentificationFormAttribute.FIELD_ATTRIBUTE).setType(QueryType.READ_MANY)
		);
	}
	
	static String getReadWhereFilterSelect() {
		return select(fields("t",IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER,IdentificationFormAttribute.FIELD_REQUIRED
				,IdentificationFormAttribute.FIELD_FORM,IdentificationFormAttribute.FIELD_ATTRIBUTE));
	}
	
	static String getReadWhereFilterForUISelect() {
		return select(
				fields("t",IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER)
				,kase(whens(
						whenEqual("t", IdentificationFormAttribute.FIELD_REQUIRED, Boolean.TRUE.toString(), "'Oui'")
						,whenEqual("t", IdentificationFormAttribute.FIELD_REQUIRED, Boolean.FALSE.toString(), "'Non'")
						), "'Hérité'")
				,fields("t",FieldHelper.join(IdentificationFormAttribute.FIELD_FORM,IdentificationForm.FIELD_NAME)
				,FieldHelper.join(IdentificationFormAttribute.FIELD_ATTRIBUTE,IdentificationAttribute.FIELD_NAME)));
	}
	
	static String getReadWhereFilterForEditSelect() {
		return select(fields("t",IdentificationFormAttribute.FIELD_IDENTIFIER,IdentificationFormAttribute.FIELD_ORDER_NUMBER,IdentificationFormAttribute.FIELD_REQUIRED
				,IdentificationFormAttribute.FIELD_FORM,IdentificationFormAttribute.FIELD_ATTRIBUTE));
	}
	
	static String getReadWhereFilterFrom() {
		return from("IdentificationFormAttribute t"
				,"LEFT JOIN IdentificationForm form ON form = t.form"
				,"LEFT JOIN IdentificationAttribute attribute ON attribute = t.attribute"
				);
	}
	
	static String getReadWhereFilterWhere() {
		return where(and(
				parenthesis(or(String.format(":%s = true", PARAMETER_NAME_FORM_IDENTIFIER_NULLABLE),"t.form.identifier = :"+PARAMETER_NAME_FORM_IDENTIFIER))
		));
	}
	
	static String getReadWhereFilterOrder() {
		return getReadOrderBy();
	}
	
	static String getReadOrderBy() {
		return order(asc("t",FieldHelper.join(IdentificationFormAttribute.FIELD_FORM,IdentificationForm.FIELD_CODE))
				,asc("t",FieldHelper.join(IdentificationFormAttribute.FIELD_ORDER_NUMBER))
				,asc("t",FieldHelper.join(IdentificationFormAttribute.FIELD_ATTRIBUTE,IdentificationForm.FIELD_CODE))
			);
	}
}