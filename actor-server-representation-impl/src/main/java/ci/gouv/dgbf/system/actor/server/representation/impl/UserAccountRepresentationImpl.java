package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.UserAccountRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.UserAccountDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class UserAccountRepresentationImpl extends AbstractRepresentationEntityImpl<UserAccountDto> implements UserAccountRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
