package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.business.api.MenuBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.MenuPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Menu;
import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

@ApplicationScoped
public class MenuBusinessImpl extends AbstractBusinessEntityImpl<Menu, MenuPersistence> implements MenuBusiness,Serializable {
	private static final long serialVersionUID = 1L;
		
}
