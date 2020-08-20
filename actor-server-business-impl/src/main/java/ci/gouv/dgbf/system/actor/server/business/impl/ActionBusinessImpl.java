package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.ActionBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ActionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class ActionBusinessImpl extends AbstractBusinessEntityImpl<Action, ActionPersistence> implements ActionBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
