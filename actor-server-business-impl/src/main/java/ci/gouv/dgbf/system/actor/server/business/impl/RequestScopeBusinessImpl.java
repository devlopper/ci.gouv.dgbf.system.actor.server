package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.RequestScopeBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.RequestScopePersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScope;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class RequestScopeBusinessImpl extends AbstractBusinessEntityImpl<RequestScope, RequestScopePersistence> implements RequestScopeBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
