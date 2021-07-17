package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true)
public class ScopeVisiblesReader extends AbstractScopeReaderImpl implements Serializable {

	private String scopeTypeCode;
	private Boolean not;
	
	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Scope.FIELD_IDENTIFIER,Scope.FIELD_IDENTIFIER);
		arguments.getTuple(Boolean.TRUE).add("Scope t");
		String visible = null;
		if(ScopeType.CODE_SECTION.equals(scopeTypeCode))
			visible = ScopeQueryStringBuilder.Predicate.hasVisibleSection(null);
		else if(ScopeType.CODE_UA.equals(scopeTypeCode))
			visible = ScopeQueryStringBuilder.Predicate.hasVisibleAdministrativeUnit(null);
		else if(ScopeType.CODE_USB.equals(scopeTypeCode))
			visible = ScopeQueryStringBuilder.Predicate.hasVisibleBudgetSpecializationUnit(null);
		else if(ScopeType.CODE_ACTION.equals(scopeTypeCode))
			visible = ScopeQueryStringBuilder.Predicate.hasVisibleAction(null);
		else if(ScopeType.CODE_ACTIVITE.equals(scopeTypeCode))
			visible = ScopeQueryStringBuilder.Predicate.hasVisibleActivity(null);
		if(StringHelper.isNotBlank(visible) && Boolean.TRUE.equals(not))
			visible = Language.Where.not(visible);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("scope visibles reader predicate", visible);
		arguments.getPredicate(Boolean.TRUE).ands("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS,visible);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Scope scope, Object[] array) {
		//Integer index = 1;
		scope.setVisible(Boolean.TRUE);
	}
}