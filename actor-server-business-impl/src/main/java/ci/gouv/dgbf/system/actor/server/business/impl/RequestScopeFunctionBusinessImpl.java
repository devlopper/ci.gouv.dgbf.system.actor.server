package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestScopeFunctionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestScopeFunctionBusinessImpl extends AbstractBusinessEntityImpl<RequestScopeFunction, RequestScopeFunctionPersistence> implements RequestScopeFunctionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
