package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;
import javax.transaction.Transactional;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.EntityLifeCycleListener;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.business.TransactionResult;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.RequestScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

@ApplicationScoped
public class RequestScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<RequestScopeFunction, RequestScopeFunctionPersistence> implements RequestScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override @Transactional
	public TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(Collection<String> scopeFunctionsIdentifiers, String actorCode) {
		ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("scope functions identifiers", scopeFunctionsIdentifiers);
		ThrowableHelper.throwIllegalArgumentExceptionIfBlank("actor code", actorCode);
		TransactionResult result = new TransactionResult().setTupleName("poste accordé");
		Integer count = RequestScopeFunctionQuerier.getInstance().updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(scopeFunctionsIdentifiers,actorCode
				,"LIBERATION",EntityLifeCycleListener.Event.UPDATE.getValue(),LocalDateTime.now(), EntityManagerGetter.getInstance().get());
		result.incrementNumberOfUpdate(count == null ? 0l : count.longValue());
		result.log(RequestScopeFunctionBusinessImpl.class);
		return result;
	}

	@Override @Transactional
	public TransactionResult updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(String actorCode,String... scopeFunctionsIdentifiers) {
		return updateGrantedToFalseWhereTrueByScopeFunctionsIdentifiers(ArrayHelper.isEmpty(scopeFunctionsIdentifiers) ? null : CollectionHelper.listOf(scopeFunctionsIdentifiers)
				, actorCode);
	}
}