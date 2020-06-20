package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.PrivilegeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.PrivilegeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class PrivilegeRepresentationImpl extends AbstractRepresentationEntityImpl<PrivilegeDto> implements PrivilegeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
