package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import javax.persistence.Query;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.ValueConverter;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.persistence.query.Filter;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ScopeVisiblesReader extends AbstractScopeVisiblesReader implements Serializable {

	private String typeCode,actorCode;
	private Boolean negate;
	
	public ScopeVisiblesReader(Filter filter) {
		if(filter != null) {
			Object visibleObject = filter.getFieldValue(ScopeQuerier.PARAMETER_NAME_VISIBLE);
			if(visibleObject instanceof String)
				visibleObject = visibleObject.equals("true");
			Boolean visible = ValueConverter.getInstance().convertToBoolean(visibleObject); 
			visible = ValueHelper.defaultToIfNull(visible,Boolean.TRUE);
			typeCode = (String)filter.getFieldValue(ScopeQuerier.PARAMETER_NAME_TYPE_CODE);
			actorCode = (String)filter.getFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE);
			negate = !visible;	
		}		
	}
	
	@Override
	protected String getVisiblePredicate() {
		return ScopeQueryStringBuilder.Predicate.scopeVisible(typeCode, StringHelper.isNotBlank(actorCode),Boolean.TRUE, negate);
	}

	@Override
	protected Boolean isRegisterable() {
		return Boolean.FALSE;
	}
	
	@Override
	protected void setQueryParameters(Query query, Collection<String> identifiers, Map<String, Object> parameters) {
		super.setQueryParameters(query, identifiers, parameters);
		if(StringHelper.isNotBlank(actorCode))
			query.setParameter(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);
	}
}